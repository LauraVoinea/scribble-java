package org.scribble.ext.gt.cli;

import org.scribble.ast.Module;
import org.scribble.ast.global.GProtoDecl;
import org.scribble.cli.CLFlags;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.global.GTGEnd;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator3;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.main.GTMain;
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

                System.out.println("---");

                Set<Role> rs = g.getRoles().stream().collect(Collectors.toSet());
                for (Role r : rs) {
                    System.out.println("project onto " + r + ": " + translate.project(rs, r).get());
                }

                System.out.println("---");

                if (!translate.isSinglePointed()) {
                    System.err.println("Not single pointed: " + translate);
                } else {
                    System.out.println("Initial g = " + translate);

                    Theta theta = new Theta(translate.getTimeoutIds());
                    System.out.println("Initial theta = " + theta);
                    foo(core, "", theta, translate, 2, new HashMap<>(), rs);
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
                g.getActs(mf, theta).stream().collect(Collectors.toMap(
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
            System.out.print("Select action [Enter]: ");  // IntelliJ terminal seems to need a prior key press (for focus?) before the first Enter
            String read = KB.nextLine();
            SAction<DynamicActionKind> a = null;
            try {
                a = as.get(Integer.parseInt(read));
            } catch (NumberFormatException x) {
                System.exit(0);
            }

            Triple<Theta, GTGType, String> p = g.step(theta, a).get();  // a in as so step is non-empty
            System.out.println(indent + p.right);
            System.out.println(indent + "a = " + a);
            System.out.println(indent + "theta = " + p.left);
            System.out.println(indent + "g = " + p.mid);
            if (!p.right.equals(GTGEnd.END)) {
                bar(core, indent + "    ", p.left, p.mid, count++);
            }
        }
    }

    // top level depth = 0
    private void foo(Core core, String indent, Theta theta, GTGType g, int depth, Map<String, Integer> unfolds, Set<Role> rs) {
        if (!g.isGood()) {
            System.err.println("Not good: " + g);
            System.exit(0);
        }
        if (!g.isCoherent()) {
            System.err.println("Not coherent: " + g);
            System.exit(0);
        }
        for (Role r : rs) {
            Optional<Pair<? extends GTLType, Sigma>> p = g.project(rs, r);
            if (!p.isPresent()) {
                System.err.println("Couldn't project onto " + r + ": " + g);
                System.exit(0);
            }
        }
        GTSModelFactory mf = (GTSModelFactory) core.config.mf.global;

        Set<SAction<DynamicActionKind>> as = g.getActs(mf, theta).stream()
                .filter(x -> !((x instanceof GTSNewTimeout) && ((GTSNewTimeout) x).n > depth))  // only bounds mixed...
                .collect(Collectors.toSet());

        System.out.println(indent + "as = " + as);
        //String read = KB.nextLine();

        for (SAction<DynamicActionKind> a : as) {
            Triple<Theta, GTGType, String> p = g.step(theta, a).get();  // a in as so step is non-empty

            boolean prune = false;
            Map<String, Integer> us = new HashMap<>(unfolds);
            for (int i = p.right.indexOf('_'); i >= 0 && i < p.right.length(); i = p.right.indexOf('_', i + 1)) {
                String recvar = p.right.substring(i + 1, p.right.indexOf(']', i + 1));
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
            if (!p.right.equals(GTGEnd.END) && !prune) {
                foo(core, indent + "    ", p.left, p.mid, depth, us, rs);
            }
        }
    }

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
