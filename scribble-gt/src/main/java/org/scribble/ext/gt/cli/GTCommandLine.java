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
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;
import org.scribble.util.ScribException;
import org.scribble.util.ScribParserException;

import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

public class GTCommandLine extends CommandLine {

    private Job job;

    public GTCommandLine(String... args) {
        super(args);
    }

    public static void main(String[] args)
            throws CommandLineException, AntlrSourceException {
        //Map<String, String> y = Stream.of("abc").collect(Collectors.toUnmodifiableMap(x -> x, x -> x));
        //y.put("def", "abc");
        //Stream.of("abc").collect(Collectors.toUnmodifiableList()).remove(0);

        GTCommandLine cl = new GTCommandLine(args);
        cl.run();
        cl.gtRun();
    }

	/*// A Scribble extension should override as appropriate
	protected Main newMain() throws ScribParserException, ScribException
	{
		Map<CoreArgs, Boolean> args = Collections.unmodifiableMap(parseCoreArgs());
		if (hasFlag(CLFlags.INLINE_MAIN_MOD_FLAG))
		{
			String inline = getUniqueFlagArgs(CLFlags.INLINE_MAIN_MOD_FLAG)[0];
			return new Main(inline, args);
		}
		else
		{
			List<Path> impaths = hasFlag(CLFlags.IMPORT_PATH_FLAG)
					? CommandLine.parseImportPaths(
							getUniqueFlagArgs(CLFlags.IMPORT_PATH_FLAG)[0])
					: Collections.emptyList();
			ResourceLocator locator = new DirectoryResourceLocator(impaths);
			Path mainpath = CommandLine
					.parseMainPath(getUniqueFlagArgs(CLFlags.MAIN_MOD_FLAG)[0]);
			return new GTMain(locator, mainpath, args);
		}
	}*/

    private boolean hasFlag(String flag) {
        return this.args.stream().anyMatch(x -> x.left.equals(flag));
    }

    private String[] getUniqueFlagArgs(String flag) {
        return this.args.stream().filter(x -> x.left.equals(flag)).findAny()
                .get().right;
    }

    protected GTMain main;  // Hack for parsed modules, should use Job instead

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
        Core core = this.job.getCore();
        //CoreContext c = core.getContext();

        Map<ModuleName, Module> parsed = this.main.getParsedModules();  // !!! Using main rather than job
        System.out.println("\n----- GT -----\n");
        System.out.println(parsed.keySet());

        for (ModuleName n : parsed.keySet()) {
            Module m = parsed.get(n);
            for (GProtoDecl g : m.getGProtoDeclChildren()) {

                /*GProtocol inlined = c.getInlined(g.getFullMemberName());
                System.out.println(inlined);
                GTGType translate = new GTGTypeTranslator2().translate(inlined.def);*/
                GTGType translate = new GTGTypeTranslator3().translate(g.getDefChild().getBlockChild().getInteractSeqChild());
                Set<Role> rs = g.getRoles().stream().collect(Collectors.toSet());

               /* System.out.println("---");

                Theta theta = new Theta(translate.getTimeoutIds());

                Set<Integer> cs = translate.getTimeoutIds();
                Map<Role, GTLConfig> locals = new HashMap<>();
                for (Role r : rs) {
                    Optional<Pair<? extends GTLType, Sigma>> opt = translate.projectTopLevel(rs, r);
                    if (!opt.isPresent()) {
                        System.err.println("Couldn't project onto " + r + ": " + translate);
                        System.exit(0);
                    }

                    Pair<? extends GTLType, Sigma> p = opt.get();
                    if (!p.right.equals(new Sigma(rs))) {
                        throw new RuntimeException("Shouldn't get here: " + p);
                    }
                    locals.put(r, new GTLConfig(r, p.left, p.right, theta));
                    System.out.println("Project onto " + r + ": " + p.left);
                }

                GTLSystem sys = new GTLSystem(locals);

                System.out.println("---");*/

                if (!translate.isSinglePointed()) {
                    System.err.println("Not single pointed: " + translate);
                } else {
                    /*System.out.println("Initial g = " + translate);
                    System.out.println("Initial theta = " + theta);
                    System.out.println("Initial locals = " + sys);*/
                    ////foo(core, "", theta, translate, 2, new HashMap<>(), rs, cs, sys);
                    //Correspondence s = new Correspondence(rs, cs, theta, translate, sys);
                    GTCorrespondence s = new GTCorrespondence(rs, translate);
                    Set<Op> com = GTUtil.umod(translate.getCommittingTop());
                    foo(core, "", s, 1, MAX, new HashMap<>(), 2, com);
                    //bar(core, "", theta, translate, 0);
                }
            }
        }

		/*System.out.println("\n-----\n");
		for (ProtoName<Global> p : c.getParsedFullnames()) {
			//System.out.println(c.getIntermediate(g));
			GTGType g = tr.translate(c.getIntermediate(p).def);
			System.out.println(p + " = " + g);
			System.out.println();
			foo(core, "", g);
		}*/
    }










    // HERE HERE ...make Correspondence class, with check method
    //        ... factor out Bounds
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










    // top level depth = 0
    private void foo(Core core, String indent, Theta theta, GTGType g, int depth,
                     Map<String, Integer> unfolds, Set<Role> rs, Set<Integer> cs, GTLSystem sys) {
        if (!rs.equals(sys.configs.keySet())) {
            throw new RuntimeException("Shouldn't get here");
        }

        if (!g.isGood()) {
            System.err.println("Not good: " + g);
            System.exit(0);
        }
        if (!g.isCoherent()) {
            System.err.println("Not coherent: " + g);
            System.exit(0);
        }

        System.out.println("\n" + indent + "Project " + g + ": ");

        Map<Role, GTLConfig> projs = new HashMap<>();
        for (Role r : rs) {
            Optional<Pair<? extends GTLType, Sigma>> opt = g.projectTop(rs, r);
            if (!opt.isPresent()) {
                System.err.println("Couldn't project onto " + r + ": " + g);
                System.exit(0);
            }
            Optional<Theta> opt_theta = g.projectTheta(cs, r);
            if (!opt_theta.isPresent()) {
                System.err.println("Couldn't project theta onto " + r + ": " + g);
                System.exit(0);
            }

            Pair<? extends GTLType, Sigma> p = opt.get();
            Theta theta_r = opt_theta.get();
            /*if (!p.right.equals(new Sigma(rs))) {
                throw new RuntimeException("Shouldn't get here: " + p);
            }*/
            GTLConfig cfg = new GTLConfig(r, p.left, p.right, theta_r);
            projs.put(r, cfg);
            System.out.println(indent + "  onto " + r + ": " + cfg);

            GTLConfig local = sys.configs.get(r);
            if (!cfg.type.equals(local.type)) {
                throw new RuntimeException("Projected type doesn't correspond with local: " + local.type);
            }
            if (!cfg.sigma.equals(local.sigma)) {
                throw new RuntimeException("Projected sigma doesn't correspond with local: " + local.sigma);
            }
            if (!cfg.theta.equals(local.theta)) {
                throw new RuntimeException("Projected theta doesn't correspond with local: " + local.theta);
            }
        }

        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;
        GTEModelFactory lmf = (GTEModelFactory) core.config.mf.local;

        Set<SAction<DynamicActionKind>> as = g.getActsTop(mf, theta).stream()
                .filter(x -> !((x instanceof GTSNewTimeout) && ((GTSNewTimeout<?>) x).n > depth))  // only bounds mixed...
                .collect(Collectors.toSet());

        System.out.println("\n" + indent + "as = " + as);
        //String read = KB.nextLine();

        for (SAction<DynamicActionKind> a : as) {
            Triple<Theta, GTGType, Tree<String>> p = g.stepTop(theta, a).getRight();  // a in as so step is non-empty

            boolean prune = false;
            Map<String, Integer> us = new HashMap<>(unfolds);
            for (int i = p.right.val.indexOf('_'); i >= 0 && i < p.right.val.length(); i = p.right.val.indexOf('_', i + 1)) {
                String recvar = p.right.val.substring(i + 1, p.right.val.indexOf(']', i + 1));
                int n = us.computeIfAbsent(recvar, x -> 0);
                us.put(recvar, n + 1);
                if (n + 1 > depth) {
                    prune = true;
                }
            }

            System.out.println(indent + p.right);
            System.out.println(indent + "a = " + a);
            System.out.println(indent + "g = " + p.mid);
            System.out.println(indent + "theta = " + p.left);
            System.out.println(indent + "unfolds = " + us);

            GTSAction cast = (GTSAction) a;
            GTEAction a_r = cast.project(lmf);

            // !!! NB subj/obj Role.EMPTY_ROLE when a_r GTSNewTimeout
            Either<Exception, Pair<GTLSystem, Tree<String>>> lstep =
                    sys.step(null, a.subj, (EAction<DynamicActionKind>) a_r);
            if (lstep.isLeft()) {
                throw new RuntimeException("Locals didn't reduce...", lstep.getLeft());
            }
            GTLSystem sys1 = lstep.getRight().left;

            System.out.println(indent + "locals = " + sys1);

            if (!p.right.equals(GTGEnd.END) && !prune) {
                foo(core, indent + "    ", p.left, p.mid, depth, us, rs, cs, sys1);
            }
        }

    }







    private static final Scanner KB = new Scanner(System.in);

    private void bar(Core core, String indent, Theta theta, GTGType g, int count) {
        if (!g.isGood()) {
            System.err.println("Not good: " + g);
            System.exit(0);
        }
        if (!g.isCoherent()) {
            System.err.println("Not coherent: " + g);
            System.exit(0);
        }
        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;

        List<Integer> tmp = new LinkedList<>();
        tmp.add(1);
        Map<Integer, SAction<DynamicActionKind>> as =
                g.getActsTop(mf, theta).stream().collect(Collectors.toMap(
                        x -> {
                            int i = tmp.remove(0);
                            tmp.add(i + 1);
                            return i;
                        },
                        x -> x,
                        (x, y) -> null,
                        LinkedHashMap::new
                ));

        if (!as.isEmpty()) {
            System.out.println("Actions: " + as.entrySet().stream().map(
                    x -> x.getKey() + "=" + x.getValue()).collect(Collectors.joining(", ")));
            int enter = -1;
            while (enter < 0) {
                String read = "";
                while (read.isEmpty()) {
                    System.out.print("Select action [Enter]: ");  // IntelliJ terminal seems to need a prior key press (for focus?) before the first Enter
                    read = KB.nextLine();
                }
                try {
                    enter = Integer.parseInt(read);
                } catch (NumberFormatException x) {
                    System.exit(0);
                }
            }
            if (enter == 0) {
                System.exit(0);
            }
            SAction<DynamicActionKind> a = as.get(enter);

            Triple<Theta, GTGType, Tree<String>> p = g.stepTop(theta, a).getRight();  // a in as so step is non-empty
            System.out.println(indent + p.right);
            System.out.println(indent + "a = " + a);
            System.out.println(indent + "theta = " + p.left);
            System.out.println(indent + "g = " + p.mid);
            if (!p.right.equals(GTGEnd.END)) {
                bar(core, indent + "    ", p.left, p.mid, count++);
            }
        }

    }









    /* ... */

    // FIXME refactor base scribble CLI methods (e.g., access Job)
    protected void runTasks()
            throws AntlrSourceException, ScribParserException, CommandLineException {
        Main mc = newMain();  // Represents current instance of tooling for given CL args
        Job job = mc.newJob();  // A Job is some series of passes performed on each Module in the MainContext (e.g., cf. Job::runVisitorPass)
        ScribException err = null;
        try {
            doValidationTasks(job);
        } catch (ScribException x) {
            err = x;
        }
		/*for (Pair<String, String[]> a : this.args) {
			CLFlag flag = this.flags.explicit.get(a.left);  // null for CLFlags.MAIN_MOD_FLAG
			if (a.left.equals(CLFlags.MAIN_MOD_FLAG) || !flag.enact) {
				continue;
			}
			if (!flag.barrier) {
				try {
					tryNonBarrierTask(job, a);
				} catch (ScribException x) {
					if (err == null) {
						err = x;
					}
				}
			} else {
				if (err == null) {
					try {
						tryBarrierTask(job, a);
					} catch (ScribException x) {
						err = x;
					}
				}
			}
		}
		if (err != null) {
			throw err;
		}*/

        this.job = job;
    }
}
