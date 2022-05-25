package org.scribble.ext.gt.cli;

import org.scribble.cli.CLFlag;
import org.scribble.cli.CLFlags;
import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.job.CoreContext;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.ProtoName;
import org.scribble.ext.gt.core.type.session.global.GTGTypeTranslator;
import org.scribble.job.Job;
import org.scribble.main.Main;
import org.scribble.main.resource.locator.DirectoryResourceLocator;
import org.scribble.main.resource.locator.ResourceLocator;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;
import org.scribble.util.ScribException;
import org.scribble.util.ScribParserException;

import java.nio.file.Path;
import java.util.List;
import java.util.Map;
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

	protected void myRun() {
		CoreContext c = this.job.getCore().getContext();
		GTGTypeTranslator tr = new GTGTypeTranslator();
		System.out.println("\n-----\n");
		for (ProtoName<Global> g : c.getParsedFullnames()) {
			//System.out.println(c.getIntermediate(g));
			System.out.println(g + " = " + tr.translate(c.getIntermediate(g).def));
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
