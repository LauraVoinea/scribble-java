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
import org.scribble.ext.gt.core.model.local.GTLConfig;
import org.scribble.ext.gt.core.model.local.GTLSystem;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.type.session.global.GTGEnd;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator3;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.main.GTMain;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.ext.gt.util.Tree;
import org.scribble.ext.gt.util.Triple;
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
        cl.gtRun();
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
            return new Main(inline, args);
        } else {
            List<Path> impaths = hasFlag(CLFlags.IMPORT_PATH_FLAG)
                    ? CommandLine
                    .parseImportPaths(getUniqueFlagArgs(CLFlags.IMPORT_PATH_FLAG)[0])
                    : Collections.emptyList();
            ResourceLocator locator = new DirectoryResourceLocator(impaths);
            Path mainpath = CommandLine
                    .parseMainPath(getUniqueFlagArgs(CLFlags.MAIN_MOD_FLAG)[0]);
            this.main = new GTMain(locator, mainpath, args);
            return this.main;
        }
    }

    protected void gtRun() {
        Core core = getJob().getCore();

        Map<ModuleName, Module> parsed = this.main.getParsedModules();  // !!! Using main rather than job
        System.out.println("\n----- GT -----\n");
        System.out.println("[GTCommandLine] Parsed modules: " + parsed.keySet());

        for (ModuleName n : parsed.keySet()) {
            Module m = parsed.get(n);

            for (GProtoDecl g : m.getGProtoDeclChildren()) {

                GTGType translate = new GTGTypeTranslator3().translate(g.getDefChild().getBlockChild().getInteractSeqChild());
                Set<Role> rs = g.getRoles().stream().collect(Collectors.toSet());

                System.out.println("\n[GTCommandLine] Translated " + g.getHeaderChild().getDeclName() + ": " + translate);

                if (!translate.isSinglePointed()) {
                    System.err.println("Not single pointed: " + translate);
                } else {
                    GTCorrespondence s = new GTCorrespondence(rs, translate);
                    Set<Op> com = GTUtil.umod(translate.getCommittingTop());

                    foo(core, "", s, 1, MAX, new HashMap<>(), 2, com);
                }
            }
        }
    }
















    /* ... global-local correspondence checking ... */

    // HERE HERE ... factor out Bounds
    //        ... do local mixed-active

    static final int MAX = 100;
    static int mystep = 1;

    private void foo(Core core, String indent, GTCorrespondence s,
                     int step, int MAX,
                     Map<String, Integer> unfolds, int depth,  // FIXME factor out bounds (just plain recs?)
                     Set<Op> com) {

        int mark = mystep;

        System.out.println("\n" + indent + "Checking (" + mystep + "):\n" + s.toString(indent));

        s.check(indent + "    ");

        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;
        GTEModelFactory lmf = (GTEModelFactory) core.config.mf.local;

        Set<SAction<DynamicActionKind>> as =

                //s.global.getActsTop(mf, s.theta).stream()
                s.global.getWeakActsTop(mf, s.theta).stream()

                        .filter(x -> !((x instanceof GTSNewTimeout<?>) && ((GTSNewTimeout<?>) x).n > depth))  // only bounds mixed...
                        .collect(Collectors.toSet());

        if (mystep >= MAX) {
            return;
        }

        System.out.println(indent + "Possible actions = " + as);
        for (SAction<DynamicActionKind> a : as) {

            System.out.println("\n" + indent + "(" + mark + "-" + step + ")\n"
                    + indent + "Stepping global: " + a);
            Triple<Theta, GTGType, Tree<String>> g_step =

                    //s.global.stepTop(s.theta, a).getRight();  // a in as so step is non-empty
                    s.global.weakStepTop(s.theta, a).getRight();  // a in as so step is non-empty

            System.out.println(g_step.right.toString(indent + "   "));

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

            System.out.println(indent + "Stepping local " + a.subj + ": " + a_r);
            // !!! NB subj/obj Role.EMPTY_ROLE when a_r GTSNewTimeout

            Either<Exception, Pair<GTLSystem, Tree<String>>> l_step =

                    //s.local.step(com, a.subj, (EAction<DynamicActionKind>) a_r);
                    s.local.weakStep(com, a.subj, (EAction<DynamicActionKind>) a_r);

            //Either.right(Pair.of(s.local, Tree.of("[WIP]")));

            if (l_step.isLeft()) {
                throw new RuntimeException("Locals stuck...", l_step.getLeft());
            }
            Pair<GTLSystem, Tree<String>> sys1 = l_step.getRight();
            System.out.println(sys1.right.toString(indent + "   "));

            //System.out.println(indent + "locals = " + sys1);

            mystep = mystep + 1;
            step = step + 1;

            GTCorrespondence s1 = new GTCorrespondence(s.roles, s.tids, g_step.left, g_step.mid, sys1.left);
            //if (!g_step.right.equals(GTGEnd.END) && !prune) {
            foo(core, incIndent(indent), s1, 1, MAX, us, depth, com);
            //}
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
