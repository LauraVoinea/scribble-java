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
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.GTCorrespondence;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.GTLSystem;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator3;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.main.GTMain;
import org.scribble.ext.gt.util.*;
import org.scribble.job.Job;
import org.scribble.main.Main;
import org.scribble.main.resource.locator.DirectoryResourceLocator;
import org.scribble.main.resource.locator.ResourceLocator;
import org.scribble.util.*;

import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class GTCommandLine extends CommandLine {

    protected GTMain main;  // Hack for parsed modules, should use Job instead

    public GTCommandLine(String... args) {
        super(args);
    }

    public static void main(String[] args) {

        GTCommandLine cl = new GTCommandLine(args);
        try {
            cl.run();
        } catch (CommandLineException | AntlrSourceException x) {
            throw new RuntimeScribException(x);
        }
        Optional<Exception> run = cl.gtRun1();
        if (run.isPresent()) {
            throw new RuntimeException(run.get());
        }
    }

    public static Optional<Exception> mainTest(String[] args) {

        GTCommandLine cl = new GTCommandLine(args);
        try {
            cl.run();
        } catch (CommandLineException | AntlrSourceException x) {
            throw new RuntimeScribException(x);
        }
        return cl.gtRun1();
    }

    @Override
    protected CLFlags newCLFlags() {
        return new GTCLFlags();
    }

    @Override
    protected void doValidationTasks(Job job)
            throws AntlrSourceException, ScribParserException,  // Latter in case needed by subclasses
            CommandLineException {
        job.runPasses();

        //job.getCore().runPasses();  // HERE HERE FIXME: base imed GTGMixedChoice visit/agg/gather overrides

    }

    // Duplicated from AssrtCommandLine
    // Based on CommandLine.newMainContext
    @Override
    protected Main newMain() throws ScribParserException, ScribException {
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

    protected Optional<Exception> gtRun1() {
        Core core = getJob().getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        Map<ModuleName, Module> parsed = this.main.getParsedModules();  // !!! Using main rather than job
        if (debug) {
            System.out.println("\n----- GT -----\n");
            System.out.println("[GTCommandLine] Parsed modules: " + parsed.keySet());
        }

        for (ModuleName n : parsed.keySet()) {
            Module m = parsed.get(n);

            for (GProtoDecl g : m.getGProtoDeclChildren()) {

                GTGType translate = new GTGTypeTranslator3().translate(
                        g.getDefChild().getBlockChild().getInteractSeqChild());
                Set<Role> rs = g.getRoles().stream().collect(Collectors.toSet());

                if (debug) {
                    System.out.println("\n[GTCommandLine] Translated "
                            + g.getHeaderChild().getDeclName() + ": " + translate);
                }

                /*if (!translate.isSinglePointed()) {  // FIXME latest global WF
                    System.err.println("Not single pointed: " + translate);
                } else*/
                if (!translate.isInitialWellSet()) {
                    return Optional.of(new Exception("Not initial and well-set: " + translate));
                }

                // initial awareness
                else if (!translate.isInitialAware(new Theta(translate.getTimeoutIds()))) {
                    return Optional.of(new Exception("Not initial awareness (single-decision): " + translate));
                    //} else if (!translate.isLeftCommitting()) {
                } else if (!translate.isLeftCommittingTop()) {
                    return Optional.of(new Exception("Not left-committing (initial awareness, clear-termination): " + translate));

                } else {
                    GTCorrespondence s = new GTCorrespondence(rs, translate);
                    Map<Integer, Pair<Set<Op>, Set<Op>>> labs = GTUtil.umod(translate.getLabels().right);
                    Set<Op> com = GTUtil.umod(translate.getCommittingTop());

                    if (!hasFlag(GTCLFlags.NO_CORRESPONDENCE)) {
                        Optional<Exception> foo = foo(core, "", s, 1, MAX, new HashMap<>(), 2, labs, com);
                        if (foo.isPresent()) {
                            return foo;
                        }
                    }
                }
            }
        }

        return Optional.empty();
    }















    /* ... global-local correspondence checking ... */

// HERE HERE ... factor out Bounds
//        ... do local mixed-active

    static final int MAX = 100;
    static int mystep = 1;

    public static void debugPrintln(boolean debug, String x) {
        if (debug) {
            System.out.println(x);
        }
    }

    private Optional<Exception> foo(Core core, String indent, GTCorrespondence s,
                                    int step, int MAX,
                                    Map<String, Integer> unfolds, int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
                                    Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
                                    Set<Op> com) {
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        int mark = mystep;

        debugPrintln(debug, "\n" + indent + "Checking (" + mystep + "):\n" + s.toString(indent));

        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;
        GTEModelFactory lmf = (GTEModelFactory) core.config.mf.local;

        Optional<Exception> check = s.check(mf, indent + "    ");
        if (check.isPresent()) {
            return check;
        }

        Set<SAction<DynamicActionKind>> as =

                //s.global.getActsTop(mf, s.theta).stream()
                s.global.getWeakActsTop(mf, s.theta).stream()

                        .filter(x -> !((x instanceof GTSNewTimeout<?>) && ((GTSNewTimeout<?>) x).n > depth))  // only bounds mixed...
                        .collect(Collectors.toSet());

        if (mystep >= MAX) {
            return Optional.empty();
        }

        debugPrintln(debug, indent + "Possible actions = " + as);
        for (SAction<DynamicActionKind> a : as) {

            debugPrintln(debug, "\n" + indent + "(" + mark + "-" + step + ")\n"
                    + indent + "Stepping global: "
                    + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTGType.weakStepTop
                    + ConsoleColors.VDASH + " " + s.global + " " + "--" + a + "--> ...");
            Triple<Theta, GTGType, Tree<String>> g_step =

                    //s.global.stepTop(s.theta, a).getRight();  // a in as so step is non-empty
                    s.global.weakStepTop(s.theta, a).getRight();  // a in as so step is non-empty

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

            debugPrintln(debug, indent + "Stepping local "
                    + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTLType.weakStepTop
                    + ConsoleColors.VDASH + " " + s.local + " --" + a.subj + ":" + a_r + "--> ...");
            // !!! NB subj/obj Role.EMPTY_ROLE when a_r GTSNewTimeout

            Either<Exception, Pair<GTLSystem, Tree<String>>> l_step =

                    //s.local.step(com, a.subj, (EAction<DynamicActionKind>) a_r);
                    s.local.weakStep(labs, com, a.subj, (EAction<DynamicActionKind>) a_r);

            //Either.right(Pair.of(s.local, Tree.of("[WIP]")));

            if (l_step.isLeft()) {
                throw new RuntimeException("Locals stuck...", l_step.getLeft());
            }
            Pair<GTLSystem, Tree<String>> sys1 = l_step.getRight();
            debugPrintln(debug, sys1.right.toString(indent + "   "));

            //System.out.println(indent + "locals = " + sys1);

            mystep = mystep + 1;
            step = step + 1;

            GTCorrespondence s1 = new GTCorrespondence(s.roles, s.tids, g_step.left, g_step.mid, sys1.left);
            //if (!g_step.right.equals(GTGEnd.END) && !prune) {
            Optional<Exception> foo = foo(core, incIndent(indent), s1, 1, MAX, us, depth, labs, com);
            if (foo.isPresent()) {
                return foo;
            }
            //}
        }

        return Optional.empty();
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
