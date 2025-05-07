package org.scribble.ext.gt.cli;

import org.scribble.ast.Module;
import org.scribble.ast.global.GProtoDecl;
import org.scribble.cli.CLFlags;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.*;
import org.scribble.ext.gt.codegen.erlang.GTGenRoleGen;
import org.scribble.ext.gt.codegen.erlang.GTRoleGen;
import org.scribble.ext.gt.codegen.java.GTJavaApiGen;
import org.scribble.ext.gt.core.model.GTCorrespondence;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.local.*;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator3;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.main.GTMain;
import org.scribble.ext.gt.util.*;
import org.scribble.job.Job;
import org.scribble.main.resource.locator.DirectoryResourceLocator;
import org.scribble.main.resource.locator.ResourceLocator;
import org.scribble.util.*;

import java.nio.file.Path;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class GTCommandLine extends CommandLine {

    protected GTMain main;  // Hack for parsed modules, should use Job instead

    public GTCommandLine(String... args) {
        super(args);
    }

    public static void main(String[] args) {
        GTCommandLine cl = init(args);
        Optional<Exception> run = cl.gtRun();
        if (run.isPresent()) {
            throw new RuntimeException(run.get());
        }
    }

    public static Optional<Exception> mainTest(String[] args) {
        GTCommandLine cl = init(args);
        return cl.gtRun();
    }

    static GTCommandLine init(String[] args) {
        GTCommandLine cl = new GTCommandLine(args);
        try {
            cl.run();
        } catch (CommandLineException | AntlrSourceException x) {
            throw new RuntimeScribException(x);
        }
        return cl;
    }

    static Map<GProtoName, GTGType> getTranslated(GTCommandLine cl) {
        Map<GProtoName, GTGType> res = new HashMap<>();

        Job job = cl.getJob();
        Core core = job.getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        /*try {
            job.runVisitorPassOnAllModules(job.config.vf.NameDisambiguator(job));  // Includes validating names used in subprotocol calls..
        } catch (ScribException e) {
            e.printStackTrace();
        }*/

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
    protected void doValidationTasks(Job job)
            throws
            AntlrSourceException, ScribParserException,  // Latter in case needed by subclasses
            CommandLineException {

        System.out.println("abcdef");

        job.runPasses();

        //job.getCore().runPasses();  // HERE HERE FIXME: base imed GTGMixedChoice visit/agg/gather overrides

    }

    /* // if -gt-api-gen is an `enact` flag
    @Override
    protected void tryBarrierTask(Job job,
                                  Pair<String, String[]> task) throws ScribException, CommandLineException {
        switch (task.left) {
            case GTCLFlags.GT_API_GEN_FLAG -> {
                /* //outputEndpointApi(job, task.right, true, true, false);
                GProtoName g = new GProtoName(task.right[0]);
                Role r = new Role(task.right[1]);
                if (this.hasFlag(GTCLFlags.GT_API_GEN_FLAG)) {
                    System.out.println("\n[GTCommandLine] API for " + r + ":\n" + new GTApiGen().generate(g, r, this.fsms.get(r)));
                }* /

                // !!! skip -- run is happening before gtRun
            }
            default -> super.tryBarrierTask(job, task);
        }
    }*/

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
            List<Path> impaths = hasFlag(CLFlags.IMPORT_PATH_FLAG)
                                 ? CommandLine
                                         .parseImportPaths(getUniqueFlagArgs(CLFlags.IMPORT_PATH_FLAG)[0])
                                 : Collections.emptyList();
            ResourceLocator locator = new DirectoryResourceLocator(impaths);
            Path mainpath = CommandLine
                    .parseMainPath(getUniqueFlagArgs(CLFlags.MAIN_MOD_FLAG)[0]);
            this.main = new GTMain(locator, mainpath, args);
        }
        return this.main;
    }

    /*protected void gtRun() {
        Core core = getJob().getCore();

        Map<ModuleName, Module> parsed = this.main.getParsedModules();  // !!! Using main rather than job
        System.out.println("\n----- GT -----\n");
        System.out.println("[GTCommandLine] Parsed modules: " + parsed.keySet());

        for (ModuleName n : parsed.keySet()) {
            Module m = parsed.get(n);

            for (GProtoDecl g : m.getGProtoDeclChildren()) {

                GTGType translate = new GTGTypeTranslator3().translate(
                        g.getDefChild().getBlockChild().getInteractSeqChild());
                Set<Role> rs = g.getRoles().stream().collect(Collectors.toSet());

                System.out.println("\n[GTCommandLine] Translated "
                        + g.getHeaderChild().getDeclName() + ": " + translate);

                /*if (!translate.isSinglePointed()) {  // FIXME latest global WF
                    System.err.println("Not single pointed: " + translate);
                } else* /
                if(!translate.isInitialWellSet())

    {
        System.err.println("Not initial and well-set: " + translate);
    }

    // initial awareness
                else if(!translate.isInitialAware(new

    Theta(translate.getTimeoutIds())))

    {
        System.err.println("Not initial awareness (single-decision): " + translate);
        //} else if (!translate.isLeftCommitting()) {
    } else if(!translate.isLeftCommittingTop())

    {
        System.err.println("Not left-committing (initial awareness, clear-termination): " + translate);

    } else

    {
        GTCorrespondence s = new GTCorrespondence(rs, translate);
        Map<Integer, Pair<Set<Op>, Set<Op>>> labs = GTUtil.umod(translate.getLabels().right);
        Set<Op> com = GTUtil.umod(translate.getCommittingTop());

        if (!hasFlag(GTCLFlags.NO_CORRESPONDENCE)) {
            foo(core, "", s, 1, MAX, new HashMap<>(), 2, labs, com);
        }
    }
}
        }
                }*/

    // TODO make checkStaticProperties -- cf. GTCorrespondence.checkRuntimeProperties
    // no messages in transit and no active timeouts.
    static Optional<Exception> checkInitialWellSet(GTGType translate) {  // "check..." vs. "is..."
        return translate.isInitialWellSet()
               ? Optional.empty() :
               Optional.of(new Exception("Not initial and well-set: " + translate));
    }

    // single-decision ensures that all non-indifferent roles depend on the timeout observer in the right-hand side of a timeout.
    static Optional<Exception> checkSingleDecision(GTGType translate) {
        Set<Role> rs = translate.getRoles();
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

    /*static Optional<Exception> checkInitialAwareness(GTGType translate) {
        // initial awareness
        Optional<Exception> res;
        res = checkSingleDecision(translate);
        if (res.isPresent()) { return res; }
        res = checkClearTermination(translate);
        return res;
    }*/

    static Optional<Exception> checkStaticProperties(GTGType translate) {
        // initial awareness
        Optional<Exception> res;
        res = checkInitialWellSet(translate);
        if (res.isPresent()) { return res; }
        res = checkSingleDecision(translate);
        if (res.isPresent()) { return res; }
        res = checkClearTermination(translate);
        return res;
    }

    //static GTCorrespondence checkProjection(GTGType translate) {
    static Either<Exception, GTCorrespondence> checkProjection(GTGType translate) {
        // Check projection -- TODO Either
        Set<Role> rs = translate.getRoles();
        Set<Integer> tids = translate.getTimeoutIds();
        Theta theta = new Theta(tids);
        Either<Exception, GTLSystem> proj = GTCorrespondence.projectTopLevel(rs, translate, tids);
        return proj.mapRight(x -> new GTCorrespondence(rs, tids, theta, translate, x));
    }

    public static GTSModelFactory GMF;
    public static GTEModelFactory LMF;

    private Map<Role, GTEState> fsms = new HashMap<>();

    // i.e., check Correspondence (modulo GTCLFlags.NO_CORRESPONDENCE flag)
    protected Optional<Exception> gtRun() {
        Core core = this.getJob().getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        GMF = (GTSModelFactory) core.config.mf.global;
        LMF = (GTEModelFactory) core.config.mf.local;

        Map<GProtoName, GTGType> translated = getTranslated(this);
        Map<String, Map<String, GTEFSM>> efsms = new HashMap<>();  // proto -> role -> EFSM
        for (GProtoName g : translated.keySet()) {
            GTGType translate = translated.get(g);
            //Set<Role> rs = translate.getRoles();
            if (debug) {
                System.out.println("\n[GTCommandLine] Translated "
                        + g + ": " + translate);
            }

            System.out.println("aaaaaaa: " + translate.unfoldAllOnce());

            /*if (!translate.isSinglePointed()) {  // FIXME latest global WF
                System.err.println("Not single pointed: " + translate);
            } else*/
            Optional<Exception> check;
            /*check = checkInitialWellSet(translate);
            if (check.isPresent()) { return check; }
            check = checkInitialAwareness(translate);
            if (check.isPresent()) { return check; }*/
            check = checkStaticProperties(translate);
            if (check.isPresent()) { return check; }

            Either<Exception, GTCorrespondence> proj = checkProjection(translate);
            if (proj.isLeft()) {
                return Optional.of(proj.getLeft());
            }
            GTCorrespondence s = proj.getRight();

            //Map<Role, Set<Op>> com = GTUtil.umod(translate.getCommittingTop());
            Map<Integer, Map<Role, Set<Op>>> comFull = translate.getCommitting();
            Map<Role, Set<Op>> com = new HashMap<>();  // deprecated
            Map<Role, Map<Integer, Set<Op>>> comInvert = new HashMap<>();
            comFull.entrySet().forEach(x -> {
                int c = x.getKey();
                Map<Role, Set<Op>> vs = x.getValue();
                for (Map.Entry<Role, Set<Op>> y : vs.entrySet()) {
                    Role r = y.getKey();
                    Set<Op> ops = y.getValue();

                    com.computeIfAbsent(r, z -> new HashSet<>()).addAll(ops);

                    Map<Integer, Set<Op>> invert = comInvert.computeIfAbsent(r, z -> new HashMap<>());
                    invert.computeIfAbsent(c, z -> new HashSet<>()).addAll(ops);
                }
            });

            System.out.println("\n[GTCommandLine] projected:\n"
                    + s.local.configs.values().stream().map(x -> x.self + "=" + x.type).collect(Collectors.joining("\n")));

            for (GTLConfig x : s.local.configs.values()) {
                GTEState init = new GTFsmConstructor().construct(com.get(x.self), x.type);
                this.fsms.put(x.self, init);
                System.out.println("\n[GTCommandLine] FSM for " + x.self + ":\n" + init.toDot());

                // !!! gtRun happens before run (i.e., tryBarrierTask running before gtRun, cf. `enact` flags`)
                if (this.hasFlag(GTCLFlags.GT_JAVA_API_GEN_FLAG)) {
                    System.out.println("\n[GTCommandLine] API for " + x.self + ":\n" + new GTJavaApiGen().generate(g, x.self, init));
                }

                GTVState s_init = new GTVState(GTVState.TOP_SCOPE);
                GTVState end = new GTVState(GTVState.TOP_SCOPE);  // !!! scope => use -1 to GC all messages (cf. separate ends per c)
                //Set<Op> com_self = com.getOrDefault(x.self, Set.of());
                Map<Integer, Set<Op>> com_self = comInvert.get(x.self);
                GTEFSM efsm = x.type.construct(x.self, com_self, Map.of(), GTVState.TOP_SCOPE, s_init, end).fix();
                System.out.println("\n[debug] EFSM: " + x.self + ": " + x.type + "\n" + efsm.toDot());
                System.out.println("\n[debug] Role gen:\n" + new GTRoleGen().generate(null, null, efsm));
                System.out.println("\n[debug] Gen role gen:\n" + new GTGenRoleGen().generate(null, null, efsm));
                Map<String, GTEFSM> tmp = efsms.computeIfAbsent(g.getSimpleName().toString(), y -> new LinkedHashMap<>());  // !!! simple name
                tmp.put(x.self.toString(), efsm);
            }

            // Check correspondence
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs = GTUtil.umod(translate.getLabels().right);
            Map<String, Integer> unfolds = translate.getRecDecls().stream()
                                                    .collect(Collectors.toMap(AbstractName::toString, x -> 0));  // FIXME don't use String
            if (!this.hasFlag(GTCLFlags.NO_CORRESPONDENCE) && !this.hasFlag(GTCLFlags.GT_NO_CORRESPONDENCE_FLAG)) {
                Optional<Exception> res =

                        // HERE HERE fidelity fine, top-down recursion TODO
                        checkExecution(  // top-down
                                //checkExecution2(  // fidelity
                                core, "", s, 1, MAX,
                                unfolds, 2,
                                translate.getTimeoutIds(),
                                labs, com,
                                true, true, true, true, true, true, true);
                if (res.isPresent()) {
                    return res;
                }
            }
        }

        /*if (this.hasFlag(GTCLFlags.GT_ED_FSM_GEN_FLAG)) {
            System.out.println("\n[GTCommandLine] event-driven FSM for: ")
        }*/
        for (Pair<String, String[]> a : this.args) {
            if (a.left.equals(GTCLFlags.GT_ED_FSM_GEN_FLAG)) {
                String proto = a.right[0];
                String r = a.right[1];
                System.out.println("\n[GTCommandLine] event-driven FSM for " + proto + "@" + r + ":");
                System.out.println(efsms.get(proto).get(r).toDot());
            } else if (a.left.equals(GTCLFlags.GT_ERLANG_API_GEN_FLAG)) {
                String proto = a.right[0];
                String r = a.right[1];
                GTEFSM m = efsms.get(proto).get(r);
                GTGenRoleGen g1 = new GTGenRoleGen();
                GTRoleGen g2 = new GTRoleGen();
                System.out.println("\n[GTCommandLine] Gen role for " + proto + "@" + r + ":");
                System.out.println(g1.generate(null, null, m));
                System.out.println("\n[GTCommandLine] Role for " + proto + "@" + r + ":");
                System.out.println(g2.generate(null, null, m));
            }
        }

        return Optional.empty();
    }















    /* ... global-local correspondence checking ... */

// HERE HERE ... factor out Bounds
//        ... do local mixed-active

    static final int MAX = 100;  // checkExecution1 top-down
    //static final int MAX = 10;  // checkExecution2 fidelity -- FIXME why slower?
    static int mystep = 1;

    // fidelity (bottom-up correspondence)
    // !!! FIXME refactor mystep ? -- add state pruning
    private static Optional<Exception> checkExecution2(
            Core core, String indent, GTCorrespondence s,
            int step, int MAX,
            Map<String, Integer> unfolds,
            int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
            Set<Integer> tids,
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
            Map<Role, Set<Op>> com,
            boolean cp, boolean ui, boolean co, boolean sd, boolean ct, boolean ac, boolean proj) {
        mystep = 1;
        return checkExecutionAux2(core, indent, s, step, MAX, unfolds, depth, tids, labs, com,
                cp, ui, co, sd, ct, ac, proj);
    }


    // HERE HERE factor out top-down/fidelity CL arg

    private static final int MAX_UNFOLD = 2;

    private static Optional<Exception> checkExecutionAux2(
            Core core, String indent, GTCorrespondence s,
            int step, int MAX,
            Map<String, Integer> unfolds,
            int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
            Set<Integer> tids,
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
            Map<Role, Set<Op>> com,
            boolean cp, boolean ui, boolean co, boolean sd, boolean ct, boolean ac, boolean proj
    ) {
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        int mark = mystep;

        debugPrintln(debug, "\n" + indent + "Checking (" + mystep + "):\n" + s.toString(indent));

        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;
        GTEModelFactory lmf = (GTEModelFactory) core.config.mf.local;

        /*for (Role r : s.roles) {
            GTLConfig p = s.local.configs.get(r);
            debugPrintln(debug, indent + "    Checking projection correspondence onto " + r + ": " + p);
        }*/
        Optional<Exception> check = s.checkProjectionCorrespondence(debug, mf, indent + "    ");
        if (check.isPresent()) {
            return check;
        }

        // cf. checkProjectionCorrespondence
        Optional<Exception> props = s.checkRuntimeProperties(mf, indent, tids, proj, cp, ui, co, sd, ct, ac);
        if (props.isPresent()) {
            return props;
        }

        // TODO factor out above with top-down correspondence checkExecutionAux

        /*Map<Role, LinkedHashSet<EAction<DynamicActionKind>>> all =
                s.local.getActs(lmf).entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        //y -> y.getValue().stream()
                        y -> y.getValue().keySet().stream()  // !!!
                                .filter(x -> !((x instanceof GTENewTimeout<?>)
                                        && ((GTENewTimeout<?>) x).n > depth)  // only bounds MCs...
                                )
                                .collect(Collectors.toCollection(LinkedHashSet::new))
                ));*/
        Map<Role, LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>>> get = s.local.getActs(lmf);
        Map<Role, LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>>> all = GTUtil.mapOf();
        for (Map.Entry<Role, LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>>> e
                : get.entrySet()) {
            Role r = e.getKey();
            LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> as = e.getValue();
            LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> filt = GTUtil.mapOf();
            for (Map.Entry<EAction<DynamicActionKind>, Set<RecVar>> e2 : as.entrySet()) {
                EAction<DynamicActionKind> a = e2.getKey();
                Set<RecVar> rvs = e2.getValue();
                if (rvs.stream().allMatch(x -> unfolds.get(x.toString()) < MAX_UNFOLD)) {  // FIXME toString
                    filt.put(a, rvs);
                }
            }
            all.put(r, filt);
        }
        //s.local.weakStep(labs, com, a.subj, (EAction<DynamicActionKind>) a_r);

        /*//s.global.getActsTop(mf, s.theta).stream()
        s.global.getWeakActsTop(mf, s.theta).stream()
                .filter(x -> !((x instanceof GTSNewTimeout<?>) && ((GTSNewTimeout<?>) x).n > depth))  // only bounds mixed...
                .collect(Collectors.toSet());*/

        //System.out.println("aaaaaaaaa: " + mystep + " ,, " + MAX);
        if (mystep >= MAX) {
            return Optional.empty();
        }

        debugPrintln(debug, indent + "Possible local actions = " + all);
        //for (SAction<DynamicActionKind> a : as) {
        //for (Map.Entry<Role, LinkedHashSet<EAction<DynamicActionKind>>> e : all.entrySet()) {
        for (Map.Entry<Role, LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>>> e : all.entrySet()) {

            Role r = e.getKey();
            //LinkedHashSet<EAction<DynamicActionKind>> as = e.getValue();
            LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> as = e.getValue();

            //for (EAction<DynamicActionKind> a : as) {
            for (Map.Entry<EAction<DynamicActionKind>, Set<RecVar>> e2 : as.entrySet()) {
                EAction<DynamicActionKind> a = e2.getKey();

                debugPrintln(debug, "\n" + indent + "(" + mark + "-" + step + ")\n"
                        + indent + "Stepping local "
                        + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTLType.weakStepTop
                        + ConsoleColors.VDASH + " " + s.local + " --" + r + ":" + a + "--> ...");
                // !!! NB subj/obj Role.EMPTY_ROLE when a_r GTSNewTimeout

                Either<Exception, Pair<GTLSystem, Tree<String>>> l_step =
                        s.local.step(com.get(r), r, a);

                //Either.right(Pair.of(s.local, Tree.of("[WIP]")));

                if (l_step.isLeft()) {
                    throw new RuntimeException("Locals stuck...", l_step.getLeft());
                }
                Pair<GTLSystem, Tree<String>> sys1 = l_step.getRight();
                debugPrintln(debug, sys1.right.toString(indent + "   "));

                //System.out.println(indent + "locals = " + sys1);

                // TODO !!! also local \nu (only if that MC already entered by other config ?)
                List<Theta> collect = sys1.left.configs.values().stream().map(x -> x.theta).collect(Collectors.toList());
                Theta t1 = collect.get(0);
                for (Theta t2 : collect.subList(1, collect.size())) {
                    Optional<Theta> opt = Theta.max(t1, t2);
                    if (opt.isEmpty()) {
                        throw new RuntimeException("Shouldn't get here? ");
                    }
                    t1 = opt.get();
                }

                /*
                GTLSystem gc = sys1.left;
                /*/
                GTLSystem ff = ffweak(lmf, com.get(r), t1, sys1.left, r);  // TODO deriv -- for multistep reductions List<Tree<...>> ?
                if (!ff.equals(sys1.left)) {
                    debugPrintln(debug, indent + "Catch up: ... --" + ConsoleColors.NU + "-" + ConsoleColors.RIGHT_ARROW + ConsoleColors.SUPER_PLUS + " " + ff);//....toString(indent + "ff " + ConsoleColors.NU + ": " + ff));
                }

                GTLSystem gc = ff.gc(labs);
                if (!gc.equals(ff)) {
                    debugPrintln(debug, indent + "GC: ... --" + ConsoleColors.TAU + "-" + ConsoleColors.RIGHT_ARROW + ConsoleColors.SUPER_PLUS + " " + gc);
                }
                //*/

                // TODO eager(?) GC
                //Pair<GTLConfig, Tree<String>> gc = cfg.gc(labs);

                debugPrintln(debug, indent + "Stepping global: "
                        + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTGType.weakStepTop
                        + ConsoleColors.VDASH + " " + s.global + " " + "--" + a + "--> ...");
                GTSAction a_g = ((GTEAction) a).mirror(mf, r);
                Triple<Theta, GTGType, Tree<String>> g_step =

                        s.global.stepTop(s.theta, (SAction<DynamicActionKind>) a_g).getRight();  // a in as so step is non-empty

                debugPrintln(debug, g_step.right.toString(indent + "   "));

                Map<String, Integer> us = new HashMap<>(unfolds);
                e2.getValue().forEach(x -> us.put(x.toString(), us.get(x.toString()) + 1));  // !!!

                mystep = mystep + 1;
                step = step + 1;

                GTCorrespondence s1 = new GTCorrespondence(
                        s.roles, s.tids, g_step.left, g_step.mid, gc);  // !!! projection corr not checked here -- checked next start of next step
                //if (!g_step.right.equals(GTGEnd.END) && !prune) {
                Optional<Exception> res = checkExecutionAux2(
                        core, incIndent(indent), s1, 1, MAX, us, depth,
                        tids, labs, com,
                        cp, ui, co, sd, ct, ac, proj);
                if (res.isPresent()) {
                    return res;
                }
                //}
            }
        }

        return Optional.empty();
    }

    private static GTLSystem ffweak(
            GTEModelFactory lmf, Set<Op> com, Theta max, GTLSystem y, Role r1) {
        Predicate<EAction<DynamicActionKind>> filt = x -> {
            if (!(x instanceof GTENewTimeout<?>)) { return false; }
            GTENewTimeout<?> cast = (GTENewTimeout<?>) x;
            return max.map.get(cast.c) > cast.n;
        };
        //for (Map.Entry<Role, LinkedHashSet<EAction<DynamicActionKind>>> e : y.getActs(lmf).entrySet()) {  // Using getActs unfolds recursion -- intentional?  cf. checking specifically GTLMixedChoice  (GTLSystem.weakStep)
        for (Map.Entry<Role, LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>>> e
                : y.getActs(lmf).entrySet()) {  // Using getActs unfolds recursion -- intentional?  cf. checking specifically GTLMixedChoice  (GTLSystem.weakStep)
            Role r = e.getKey();
            Optional<EAction<DynamicActionKind>> opt =
                    //e.getValue().stream().filter(filt::test).findFirst();
                    e.getValue().keySet().stream().filter(filt::test).findFirst();  // !!!
            if (opt.isPresent()) {
                Either<Exception, Pair<GTLSystem, Tree<String>>> step = y.step(com, r, opt.get());
                if (step.isLeft()) {
                    throw new RuntimeException("Shouldn't get here: " + step.getLeft());
                }
                return ffweak(lmf, com, max, step.getRight().left, r);  // TODO deriv
            }
        }
        return y;
    }


    /* ... */

    // top-down (cf. fidelity bottom-up) -- "old style" weak (GC as pre-action)
    // !!! FIXME refactor mystep ? -- add state pruning
    private static Optional<Exception> checkExecution(
            Core core, String indent, GTCorrespondence s,
            int step, int MAX,
            Map<String, Integer> unfolds,
            int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
            Set<Integer> tids,
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
            Map<Role, Set<Op>> com,
            boolean cp, boolean ui, boolean co, boolean sd, boolean ct, boolean ac, boolean proj) {
        mystep = 1;
        return checkExecutionAux(core, indent, s, step, MAX, unfolds, depth, tids, labs, com,
                cp, ui, co, sd, ct, ac, proj);
    }

    private static Optional<Exception> checkExecutionAux(
            Core core, String indent, GTCorrespondence s,
            int step, int MAX,
            Map<String, Integer> unfolds,
            int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
            Set<Integer> tids,
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
            Map<Role, Set<Op>> com,
            boolean cp, boolean ui, boolean co, boolean sd, boolean ct, boolean ac, boolean proj
    ) {
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        int mark = mystep;

        debugPrintln(debug, "\n" + indent + "Checking (" + mystep + "):\n" + s.toString(indent));

        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;
        GTEModelFactory lmf = (GTEModelFactory) core.config.mf.local;

        /*for (Role r : s.roles) {
            GTLConfig p = s.local.configs.get(r);
            debugPrintln(debug, indent + "    Checking projection correspondence onto " + r + ": " + p);
        }*/
        Optional<Exception> check = s.checkProjectionCorrespondence(debug, mf, indent + "    ");
        if (check.isPresent()) {
            return check;
        }

        // cf. checkProjectionCorrespondence
        Optional<Exception> props = s.checkRuntimeProperties(mf, indent, tids, proj, cp, ui, co, sd, ct, ac);
        if (props.isPresent()) {
            return props;
        }

        // HERE HERE infinite global \nu ?
        /*Set<SAction<DynamicActionKind>> as =

                //s.global.getActsTop(mf, s.theta).stream()
                s.global.getWeakActsTop(mf, s.theta).stream()

                        .filter(x -> !((x instanceof GTSNewTimeout<?>) && ((GTSNewTimeout<?>) x).n > depth))  // only bounds mixed...
                        .collect(Collectors.toSet());*/

        LinkedHashMap<SAction<DynamicActionKind>, Set<RecVar>> get = s.global.getActsTop(mf, s.theta);
        LinkedHashMap<SAction<DynamicActionKind>, Set<RecVar>> filt = GTUtil.mapOf();
        //LinkedHashMap<SAction<DynamicActionKind>, Set<RecVar>> all = filt;
        for (Map.Entry<SAction<DynamicActionKind>, Set<RecVar>> e
                : get.entrySet()) {
            //LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> as = e.getValue();
            //for (Map.Entry<SAction<DynamicActionKind>, Set<RecVar>> e2 : as.entrySet()) {
            SAction<DynamicActionKind> a = e.getKey();
            Set<RecVar> rvs = e.getValue();
            if (rvs.stream().allMatch(x -> unfolds.get(x.toString()) < MAX_UNFOLD)) {  // FIXME toString
                filt.put(a, rvs);
            }
            //}
            //all.put(r, filt);
        }
        Set<SAction<DynamicActionKind>> as = filt.keySet();

        if (mystep >= MAX) {
            return Optional.empty();
        }

        debugPrintln(debug, indent + "Possible global actions = " + as);
        for (SAction<DynamicActionKind> a : as) {

            debugPrintln(debug, "\n" + indent + "(" + mark + "-" + step + ")\n"
                    + indent + "Stepping global: "
                    + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTGType.weakStepTop
                    + ConsoleColors.VDASH + " " + s.global + " " + "--" + a + "--> ...");
            Triple<Theta, GTGType, Tree<String>> g_step =

                    //s.global.weakStepTop(s.theta, a).getRight();  // a in as so step is non-empty
                    s.global.stepTop(s.theta, a).getRight();  // a in as so step is non-empty

            debugPrintln(debug, g_step.right.toString(indent + "   "));

            //boolean prune = false;
            Map<String, Integer> us = new HashMap<>(unfolds);
            /*for (int i = g_step.right.indexOf('_'); i >= 0 && i < g_step.right.length();
                 i = g_step.right.indexOf('_', i + 1)) {
                String recvar = g_step.right.substring(i + 1, g_step.right.indexOf(']', i + 1));
                int n = us.computeIfAbsent(recvar, x -> 0);
                us.put(recvar, n + 1);
                if (n + 1 > depth) {
                    prune = true;
                }
            }*/

            /*System.out.println(indent + g_step.right);
            System.out.println(indent + "a = " + a);
            System.out.println(indent + "g = " + g_step.mid);
            System.out.println(indent + "theta = " + g_step.left);
            System.out.println(indent + "unfolds = " + us);*/

            GTSAction cast = (GTSAction) a;
            GTEAction a_r = cast.project(lmf);
            Pair<GTLSystem, Tree<String>> sys1;

            debugPrintln(debug, indent + "Stepping local "
                    + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTLType.weakStepTop
                    + ConsoleColors.VDASH + " " + s.local + " --" + a.subj + ":" + a_r + "--> ...");
            // !!! NB subj/obj Role.EMPTY_ROLE when a_r GTSNewTimeout
            if (a_r instanceof GTENewTimeout) {
                GTLSystem ff = ffweak(lmf, Collections.emptySet(), g_step.left, s.local, null);  // !!! g_step.left  // CHECKME empty com
                sys1 = new Pair<>(ff, Tree.of("[FF] ... --> " + ff.toString()));
            } else {
                Either<Exception, Pair<GTLSystem, Tree<String>>> l_step =

                        ////s.local.step(com, a.subj, (EAction<DynamicActionKind>) a_r);
                        //s.local.weakStep(labs, com, a.subj, (EAction<DynamicActionKind>) a_r);
                        s.local.step(com.get(a.subj), a.subj, (EAction<DynamicActionKind>) a_r);

                //Either.right(Pair.of(s.local, Tree.of("[WIP]")));

                if (l_step.isLeft()) {
                    throw new RuntimeException("Locals stuck...", l_step.getLeft());
                }
                sys1 = l_step.getRight();
            }

            debugPrintln(debug, sys1.right.toString(indent + "   "));

            //System.out.println(indent + "locals = " + sys1);

            mystep = mystep + 1;
            step = step + 1;

            GTCorrespondence s1 = new GTCorrespondence(
                    s.roles, s.tids, g_step.left, g_step.mid, sys1.left);  // !!! projection corr not checked here -- checked next start of next step
            //if (!g_step.right.equals(GTGEnd.END) && !prune) {
            Optional<Exception> res = checkExecutionAux(
                    core, incIndent(indent), s1, 1, MAX, us, depth,
                    tids, labs, com,
                    cp, ui, co, sd, ct, ac, proj);
            if (res.isPresent()) {
                return res;
            }
            //}
        }

        return Optional.empty();
    }


    /* ... */

    public static void debugPrintln(boolean debug, String x) {
        if (debug) {
            System.out.println(x);
        }
    }

    static String incIndent(String indent) {
        return indent.equals("") ? "    " : indent + ".   ";
    }

    private boolean hasFlag(String flag) {
        return this.args.stream().anyMatch(x -> x.left.equals(flag));
    }

    private String[] getUniqueFlagArgs(String flag) {
        return this.args.stream()
                        .filter(x -> x.left.equals(flag)).findAny().get().right;
    }

}
