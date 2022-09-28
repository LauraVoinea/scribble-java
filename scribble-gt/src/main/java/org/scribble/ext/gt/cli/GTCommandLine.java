package org.scribble.ext.gt.cli;

import org.scribble.ast.Module;
import org.scribble.ast.global.GInteractionSeq;
import org.scribble.ast.global.GProtoDecl;
import org.scribble.ast.global.GProtoDef;
import org.scribble.cli.CLFlag;
import org.scribble.cli.CLFlags;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.job.Core;
import org.scribble.core.job.CoreArgs;
import org.scribble.core.job.CoreContext;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.ModuleName;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.global.GTGEnd;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator2;
import org.scribble.ext.gt.main.GTMain;
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
import java.util.stream.Stream;

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
		cl.myRun();
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

	protected GTMain main;

	// Duplicated from AssrtCommandLine
	// Based on CommandLine.newMainContext
	@Override
	protected Main newMain() throws ScribParserException, ScribException
	{
		/*if (!hasFlag(AssrtCLFlags.ASSRT_CORE_FLAG))
		{
			return super.newMain();
		}*/
		//AssrtCoreArgs args = newCoreArgs();
		CoreArgs args = newCoreArgs();
		List<Path> impaths = parseImportPaths();
		ResourceLocator locator = new DirectoryResourceLocator(impaths);
		Path mainpath = parseMainPath();
		this.main = new GTMain(locator, mainpath, args);
		return main;
	}

	protected void myRun() {
		Core core = this.job.getCore();
		CoreContext c = core.getContext();

		Map<ModuleName, Module> parsed = this.main.getParsedModules();
		System.out.println("\n-----\n");
		System.out.println(parsed.keySet());

		for (ModuleName n : parsed.keySet()) {
			Module m = parsed.get(n);
			for (GProtoDecl g : m.getGProtoDeclChildren()) {
				System.out.println(g);

				GProtoDef defChild = g.getDefChild();
				GInteractionSeq interactSeqChild = defChild.getBlockChild().getInteractSeqChild();
				GTGType translate = new GTGTypeTranslator2().translate(interactSeqChild);

				System.out.println("---");

				for (Role r : g.getRoles()) {
					System.out.println("project onto " + r + ": " + translate.project(r));
				}

				System.out.println("---");

				if (!translate.isSinglePointed()) {
					System.err.println("Not single pointed: " + translate);
				} else {
					foo(core, "", translate, 0);
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

	private void foo(Core core, String indent, GTGType g, int count) {
		if (!g.isGood()) {
			System.err.println("Not good: " + g);
			System.exit(0);
		}
		if (!g.isCoherent()) {
			System.err.println("Not coherent: " + g);
			System.exit(0);
		}
		if (count > 20) {
			System.err.println("Pruned.");
			return;
		}
		Set<SAction> as = g.getActs(core.config.mf.global);
		/*System.out.println(as);
		SAction a = as.iterator().next();
		GTGType g1 = g.step(a).get();  // a in as so step is non-empty
		System.out.println("aaa: " + a + " ,, " + g1);
		System.out.println(indent + "a = " + a);
		System.out.println(indent + "g = " + g1);

		System.out.println("---");

		as = g1.getActs(core.config.mf.global);
		System.out.println(as);
		a = as.iterator().next();
		g1 = g1.step(a).get();  // a in as so step is non-empty
		System.out.println("aaa: " + a + " ,, " + g1);
		System.out.println(indent + "a = " + a);
		System.out.println(indent + "g = " + g1);*/

		for (SAction a : as) {
			//System.out.println("bbb: " + g + " ,, " + a);
			GTGType g1 = g.step(a).get();  // a in as so step is non-empty
			System.out.println(indent + "a = " + a);
			System.out.println(indent + "g = " + g1);
			if (!g1.equals(GTGEnd.END)) {
				foo(core, indent + "    ", g1, count++);
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
