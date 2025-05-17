package org.scribble.ext.gt.cli;

import org.scribble.ast.Module;
import org.scribble.ast.global.GProtoDecl;
import org.scribble.cli.CLFlags;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.type.name.*;
import org.scribble.ext.gt.codegen.erlang.GTGenRoleGen;
import org.scribble.ext.gt.codegen.erlang.GTRoleGen;
import org.scribble.ext.gt.core.model.GTCorrespondence;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.*;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator3;
import org.scribble.ext.gt.main.GTMain;
import org.scribble.ext.gt.util.*;
import org.scribble.job.Job;
import org.scribble.main.resource.locator.DirectoryResourceLocator;
import org.scribble.main.resource.locator.ResourceLocator;
import org.scribble.util.*;

import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class GTCommandLine2 extends CommandLine {

    // CHECKME probably will be used by AST classes?
    public static GTSModelFactory GMF;
    public static GTEModelFactory LMF;

    // i.e., check Correspondence (modulo GTCLFlags.NO_CORRESPONDENCE flag)
    protected Optional<Exception> gtMain() {
        Core core = this.getJob().getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);
        GTCommandLine2.GMF = (GTSModelFactory) core.config.mf.global;
        GTCommandLine2.LMF = (GTEModelFactory) core.config.mf.local;

        Map<GProtoName, GTGType> translated = getTranslated(this);

        Map<GProtoName, Map<Role, GTEFSM>> efsms = new HashMap<>();
        for (GProtoName g : translated.keySet()) {
            GTGType translate = translated.get(g);
            if (debug) {
                System.out.println("\n[GTCommandLine] Translated " + g + ": " + translate);
            }

            Optional<Exception> check = checkStaticProperties(debug, translate);
            if (check.isPresent()) { return check; }

            Either<Exception, GTCorrespondence> proj = checkProjection(translate);
            if (proj.isLeft()) { return Optional.of(proj.getLeft()); }
            GTCorrespondence s = proj.getRight();
            if (debug) {
                System.out.println("\n[GTCommandLine] projected:\n"
                        + s.local.configs.values().stream()
                                         .map(x -> x.self + "=" + x.type)
                                         .collect(Collectors.joining("\n")));
            }

            // Integer is mixed-choice ID `c`
            GProtoName simple = g.getSimpleName();  // TODO replace by fully qualified
            Map<Integer, Map<Role, Set<Op>>> comFull = translate.getCommitting();
            Map<Role, Map<Integer, Set<Op>>> comInvert = getComInvert(comFull);

            Map<Role, GTEFSM> tmp = getEFSMS(s.local, comInvert);
            efsms.put(simple, tmp);
            if (debug) {
                for (Map.Entry<Role, GTEFSM> x : tmp.entrySet()) {
                    Role r = x.getKey();
                    GTEFSM efsm = x.getValue();
                    System.out.println("\n[debug] EFSM: " + r + ": " + s.local.configs.get(r) + "\n" + efsm.toDot());
                    System.out.println("\n[debug] Role gen:\n" + new GTRoleGen().generate(simple, r, efsm));
                    System.out.println("\n[debug] Gen role gen:\n" + new GTGenRoleGen().generate(simple, r, efsm));
                }
            }
        }

        for (Pair<String, String[]> a : this.args) {
            if (a.left.equals(GTCLFlags.GT_ED_FSM_GEN_FLAG)) {
                outEFSM(efsms, a);
            } else if (a.left.equals(GTCLFlags.GT_ERLANG_API_GEN_FLAG)) {
                outAPI(efsms, a);
            }
        }

        return Optional.empty();
    }

    static Map<Role, Map<Integer, Set<Op>>> getComInvert(Map<Integer, Map<Role, Set<Op>>> comFull) {
        Map<Role, Map<Integer, Set<Op>>> comInvert = new HashMap<>();
        comFull.entrySet().forEach(x -> {
            int c = x.getKey();
            Map<Role, Set<Op>> vs = x.getValue();
            for (Map.Entry<Role, Set<Op>> y : vs.entrySet()) {
                Role r = y.getKey();
                Set<Op> ops = y.getValue();
                Map<Integer, Set<Op>> invert =
                        comInvert.computeIfAbsent(r, z -> new HashMap<>());
                invert.computeIfAbsent(c, z -> new HashSet<>()).addAll(ops);
            }
        });
        return comInvert;
    }

    // !!! GTLSystem (cf. just local types)
    static Map<Role, GTEFSM> getEFSMS(GTLSystem local, Map<Role, Map<Integer, Set<Op>>> comInvert) {
        Map<Role, GTEFSM> efsms = new HashMap<>();
        for (GTLConfig x : local.configs.values()) {
            GTVState s_init = new GTVState(GTVState.TOP_SCOPE);
            GTVState end = new GTVState(GTVState.TOP_SCOPE);  // !!! scope => use -1 to GC all messages (cf. separate ends per c)
            Map<Integer, Set<Op>> com_self = comInvert.getOrDefault(x.self, Collections.emptyMap());
            GTEFSM efsm = x.type.construct(x.self, com_self, Map.of(), GTVState.TOP_SCOPE, s_init, end).fix();
            efsms.put(x.self, efsm);
        }
        return efsms;
    }

    static void outEFSM(Map<GProtoName, Map<Role, GTEFSM>> efsms, Pair<String, String[]> a) {
        GProtoName simple = new GProtoName(a.right[0]);
        Role r = new Role(a.right[1]);
        System.out.println("\n[GTCommandLine] event-driven FSM for " + simple + "@" + r + ":");
        System.out.println(efsms.get(simple).get(r).toDot());
    }

    static void outAPI(Map<GProtoName, Map<Role, GTEFSM>> efsms, Pair<String, String[]> a) {
        GProtoName simple = new GProtoName(a.right[0]);
        Role r = new Role(a.right[1]);
        GTEFSM m = efsms.get(simple).get(r);
        GTGenRoleGen g1 = new GTGenRoleGen();
        GTRoleGen g2 = new GTRoleGen();
        System.out.println("\n[GTCommandLine] Gen role for " + simple + "@" + r + ":");
        System.out.println(g1.generate(simple, r, m));
        System.out.println("\n[GTCommandLine] Role for " + simple + "@" + r + ":");
        System.out.println(g2.generate(simple, r, m));
    }







    /* Well formedness */

    static Optional<Exception> checkStaticProperties(boolean debug, GTGType translate) {
        /*// OLD
        // initial awareness
        Optional<Exception> res;
        res = checkInitialWellSet(translate);
        if (res.isPresent()) { return res; }
        res = checkSingleDecision(translate);
        if (res.isPresent()) { return res; }
        res = checkClearTermination(translate);
        //return res;
        if (res.isPresent()) {
            throw new RuntimeException(res.get());
        }*/

        // initial
        // well-formed
        // aware  !! white triangle
        // balanced

        if (debug) {
            System.out.println("\naaaaa initial and p->q: " + translate.isInitialAndpq());
            System.out.println("bbbbb committing: " + translate.getCommittingNew());
            System.out.println("ccccc strict deps: " + translate.getStrictSyntacticDeps());
            System.out.println("ddddd aware: " + translate.isSyntacticAware());
            System.out.println("eeeee balanced: " + translate.isBalanced());
        }

        if (!translate.isInitialAndpq()) {
            return Optional.of(new Exception("Not initial with correct other/observer prefixes: " + translate));
        }

        GTGType unfolded = translate.unfoldAllOnce();
        if (debug) {
            System.out.println("\n[GTCommandLine] Unfolded all once: " + unfolded);
        }
        Optional<Exception> wf = unfolded.checkWellFormed();
        if (wf.isPresent()) { return wf; }

        Optional<Exception> aware = translate.isSyntacticAware();
        if (aware.isPresent()) {
            //return Optional.of(new Exception("Not aware: " + translate));
            return aware;
        }

        if (!translate.isBalanced()) {
            return Optional.of(new Exception("Not balanced: " + translate));
        }

        return Optional.empty();
    }




    // OLD

    // TODO make checkStaticProperties -- cf. GTCorrespondence.checkRuntimeProperties
    // no messages in transit and no active timeouts.
    static Optional<Exception> checkInitialWellSet(GTGType translate) {  // "check..." vs. "is..."
        return translate.isInitialWellSet()
               ? Optional.empty() :
               Optional.of(new Exception("Not initial and well-set: " + translate));
    }

    // single-decision ensures that all non-indifferent roles depend on the timeout observer in the right-hand side of a timeout.
    static Optional<Exception> checkSingleDecision(GTGType translate) {
        Set<Role> rs = translate.getLiveRoles();
        if (!translate.isSingleDecision(rs, new Theta(translate.getTimeoutIds()))) {
            return Optional.of(new Exception("Not single-decision: " + translate));
            //} else if (!translate.isLeftCommitting()) {
        }
        return Optional.empty();
    }

    // Clear-termination requires that all participants are eventually notified that the left-hand side branch is taken.
    static Optional<Exception> checkClearTermination(GTGType translate) {
        if (!translate.isClearTermination()) {
            return Optional.of(new Exception("Not left-committing (clear-termination): " + translate));
        }
        return Optional.empty();
    }


    /* Projection */

    //static GTCorrespondence checkProjection(GTGType translate) {
    static Either<Exception, GTCorrespondence> checkProjection(GTGType translate) {
        // Check projection -- TODO Either
        Set<Role> rs = translate.getLiveRoles();
        Set<Integer> tids = translate.getTimeoutIds();
        Theta theta = new Theta(tids);
        Either<Exception, GTLSystem> proj = GTCorrespondence.projectTopLevel(rs, translate, tids);
        return proj.mapRight(x -> new GTCorrespondence(rs, tids, theta, translate, x));
    }

























    /* Parent Scribble stuff */

    protected GTMain main;  // Hack for parsed modules, should use Job instead

    public GTCommandLine2(String... args) {
        super(args);
    }

    public static void main(String[] args) {
        GTCommandLine2 cl = init(args);
        Optional<Exception> run = cl.gtMain();
        if (run.isPresent()) {
            throw new RuntimeException(run.get());
        }
    }

    public static Optional<Exception> mainTest(String[] args) {
        GTCommandLine2 cl = init(args);
        return cl.gtMain();
    }

    static GTCommandLine2 init(String[] args) {
        GTCommandLine2 cl = new GTCommandLine2(args);
        try {
            cl.run();
        } catch (CommandLineException | AntlrSourceException x) {
            throw new RuntimeScribException(x);
        }
        return cl;
    }

    static Map<GProtoName, GTGType> getTranslated(GTCommandLine2 cl) {
        Map<GProtoName, GTGType> res = new HashMap<>();

        Job job = cl.getJob();
        Core core = job.getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        //Map<ModuleName, Module> parsed = cl.main.getParsedModules();  // XXX original source, no disamb
        Map<ModuleName, Module> parsed = job.getContext().getParsed();  // !!! post disamb
        if (debug) {
            System.out.println("\n----- GT -----\n");
            System.out.println("[GTCommandLine] Parsed modules: " + parsed.keySet());
        }

        for (ModuleName n : parsed.keySet()) {
            Module m = parsed.get(n);
            for (GProtoDecl g : m.getGProtoDeclChildren()) {
                GTGType translate = new GTGTypeTranslator3().translate(
                        g.getDefChild().getBlockChild().getInteractSeqChild());
                if (debug) {
                    System.out.println("\n[GTCommandLine] Translated "
                            + g.getHeaderChild().getDeclName() + ": " + translate);
                }
                res.put(g.getFullMemberName(parsed.get(n)), translate);
            }
        }
        return res;
    }

    @Override
    protected CLFlags newCLFlags() {
        return new GTCLFlags();
    }

    @Override
    protected void doValidationTasks(Job job) throws
            AntlrSourceException, ScribParserException,  // Latter in case needed by subclasses
            CommandLineException {
        job.runPasses();

        //job.getCore().runPasses();  // HERE HERE FIXME: base imed GTGMixedChoice visit/agg/gather overrides
    }

    @Override
    protected void tryBarrierTask(Job job, Pair<String, String[]> task) throws ScribException, CommandLineException {
        // `run` happens before `gtRun` -- skip GT flags in `run`
        switch (task.left) {
            case GTCLFlags.GT_ED_FSM_GEN_FLAG:
                break;
            case GTCLFlags.GT_ERLANG_API_GEN_FLAG:
                break;
            default:
                super.tryBarrierTask(job, task);
        }
    }


    // Duplicated from AssrtCommandLine
    // Based on CommandLine.newMainContext
    @Override
    protected GTMain newMain() throws ScribParserException, ScribException {
        Map<CoreArgs, Boolean> args = Collections.unmodifiableMap(parseCoreArgs());
        if (hasFlag(CLFlags.INLINE_MAIN_MOD_FLAG)) {
            String inline = getUniqueFlagArgs(CLFlags.INLINE_MAIN_MOD_FLAG)[0];
            this.main = new GTMain(inline, args);
        } else {
            List<Path> impaths =
                    hasFlag(CLFlags.IMPORT_PATH_FLAG)
                    ? CommandLine.parseImportPaths(getUniqueFlagArgs(CLFlags.IMPORT_PATH_FLAG)[0])
                    : Collections.emptyList();
            ResourceLocator locator = new DirectoryResourceLocator(impaths);
            Path mainpath = CommandLine
                    .parseMainPath(getUniqueFlagArgs(CLFlags.MAIN_MOD_FLAG)[0]);
            this.main = new GTMain(locator, mainpath, args);
        }
        return this.main;
    }


    /* Aux for parent Scribble stuff */

    private boolean hasFlag(String flag) {
        return this.args.stream().anyMatch(x -> x.left.equals(flag));
    }

    private String[] getUniqueFlagArgs(String flag) {
        return this.args.stream()
                        .filter(x -> x.left.equals(flag)).findAny().get().right;
    }
}
