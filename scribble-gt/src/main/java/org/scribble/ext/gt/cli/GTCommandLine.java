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
import org.scribble.core.type.name.GProtoName;
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
import java.util.stream.Collectors;

public class GTCommandLine extends CommandLine {

    protected GTMain main;  // Hack for parsed modules, should use Job instead

    public GTCommandLine(String... args) {
        super(args);
    }

    public static void main(String[] args) {
        GTCommandLine cl = init(args);
        Optional<Exception> run = gtRun(cl);
        if (run.isPresent()) {
            throw new RuntimeException(run.get());
        }
    }

    public static Optional<Exception> mainTest(String[] args) {
        GTCommandLine cl = init(args);
        return gtRun(cl);
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

        Core core = cl.getJob().getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        Map<ModuleName, Module> parsed = cl.main.getParsedModules();  // !!! Using main rather than job
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
        job.runPasses();

        //job.getCore().runPasses();  // HERE HERE FIXME: base imed GTGMixedChoice visit/agg/gather overrides

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
        if (!translate.isSingleDecision(new Theta(translate.getTimeoutIds()))) {
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

    // i.e., check Correspondence (modulo GTCLFlags.NO_CORRESPONDENCE flag)
    protected static Optional<Exception> gtRun(GTCommandLine cl) {
        Core core = cl.getJob().getCore();
        boolean debug = core.config.hasFlag(CoreArgs.VERBOSE);

        Map<GProtoName, GTGType> translated = getTranslated(cl);
        for (GProtoName g : translated.keySet()) {
            GTGType translate = translated.get(g);
            //Set<Role> rs = translate.getRoles();
            if (debug) {
                System.out.println("\n[GTCommandLine] Translated "
                        + g + ": " + translate);
            }

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

            // Check correspondence
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs = GTUtil.umod(translate.getLabels().right);
            Set<Op> com = GTUtil.umod(translate.getCommittingTop());
            if (!cl.hasFlag(GTCLFlags.NO_CORRESPONDENCE)) {
                Optional<Exception> res =

                        //checkExecution(  // top-down
                        checkExecution2(  // fidelity
                                core, "", s, 1, MAX,
                                new HashMap<>(), 2,
                                translate.getTimeoutIds(),
                                labs, com,
                                true, true, true, true, true, true, true);
                if (res.isPresent()) {
                    return res;
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

    // fidelity (bottom-up correspondence)
    // !!! FIXME refactor mystep ? -- add state pruning
    private static Optional<Exception> checkExecution2(
            Core core, String indent, GTCorrespondence s,
            int step, int MAX,
            Map<String, Integer> unfolds,
            int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
            Set<Integer> tids,
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
            Set<Op> com,
            boolean cp, boolean ui, boolean co, boolean sd, boolean ct, boolean ac, boolean proj) {
        mystep = 1;
        return checkExecutionAux2(core, indent, s, step, MAX, unfolds, depth, tids, labs, com,
                cp, ui, co, sd, ct, ac, proj);
    }


    // HERE HERE factor out top-down/fidelity CL arg


    private static Optional<Exception> checkExecutionAux2(
            Core core, String indent, GTCorrespondence s,
            int step, int MAX,
            Map<String, Integer> unfolds,
            int depth,  // depth is TOs -- only need unfolds? (though LTS rec squashed) -- FIXME factor out bounds (depth+seen, cf. EA)
            Set<Integer> tids,
            Map<Integer, Pair<Set<Op>, Set<Op>>> labs,
            Set<Op> com,
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

        Map<Role, LinkedHashSet<EAction<DynamicActionKind>>> all =
                s.local.getActs(lmf).entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        y -> y.getValue().stream()
                                .filter(x -> !((x instanceof GTENewTimeout<?>) && ((GTENewTimeout<?>) x).n > depth))  // only bounds mixed...
                                .collect(Collectors.toCollection(LinkedHashSet::new))
                ));
        //s.local.weakStep(labs, com, a.subj, (EAction<DynamicActionKind>) a_r);

        //s.global.getActsTop(mf, s.theta).stream()
        s.global.getWeakActsTop(mf, s.theta).stream()
                .filter(x -> !((x instanceof GTSNewTimeout<?>) && ((GTSNewTimeout<?>) x).n > depth))  // only bounds mixed...
                .collect(Collectors.toSet());

        if (mystep >= MAX) {
            return Optional.empty();
        }

        debugPrintln(debug, indent + "Possible actions = " + all);
        //for (SAction<DynamicActionKind> a : as) {
        for (Map.Entry<Role, LinkedHashSet<EAction<DynamicActionKind>>> e : all.entrySet()) {

            Role r = e.getKey();
            LinkedHashSet<EAction<DynamicActionKind>> as = e.getValue();

            for (EAction<DynamicActionKind> a : as) {

                debugPrintln(debug, "\n" + indent + "(" + mark + "-" + step + ")\n"
                        + indent + "Stepping local "
                        + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTLType.weakStepTop
                        + ConsoleColors.VDASH + " " + s.local + " --" + r + ":" + a + "--> ...");
                // !!! NB subj/obj Role.EMPTY_ROLE when a_r GTSNewTimeout

                Either<Exception, Pair<GTLSystem, Tree<String>>> l_step =
                        s.local.step(com, r, a);

                //Either.right(Pair.of(s.local, Tree.of("[WIP]")));

                if (l_step.isLeft()) {
                    throw new RuntimeException("Locals stuck...", l_step.getLeft());
                }
                Pair<GTLSystem, Tree<String>> sys1 = l_step.getRight();
                debugPrintln(debug, sys1.right.toString(indent + "   "));

                //System.out.println(indent + "locals = " + sys1);

                debugPrintln(debug, indent + "Stepping global: "
                        + GTLType.c_TOP + ", " + GTLType.n_INIT + " "  // cf. GTGType.weakStepTop
                        + ConsoleColors.VDASH + " " + s.global + " " + "--" + a + "--> ...");
                GTSAction a_g = ((GTEAction) a).mirror(mf, r);
                Triple<Theta, GTGType, Tree<String>> g_step =

                        s.global.stepTop(s.theta, (SAction<DynamicActionKind>) a_g).getRight();  // a in as so step is non-empty

                debugPrintln(debug, g_step.right.toString(indent + "   "));

                Map<String, Integer> us = new HashMap<>(unfolds);

                mystep = mystep + 1;
                step = step + 1;

                GTCorrespondence s1 = new GTCorrespondence(
                        s.roles, s.tids, g_step.left, g_step.mid, sys1.left);  // !!! projection corr not checked here -- checked next start of next step
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
            Set<Op> com,
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
            Set<Op> com,
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
