package org.scribble.ext.f17;

import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;

import org.scribble.ast.Module;
import org.scribble.ast.global.GProtocolDecl;
import org.scribble.del.ModuleDel;
import org.scribble.f17.ast.global.F17GProtocolDeclTranslator;
import org.scribble.f17.ast.global.F17GType;
import org.scribble.f17.ast.local.F17LType;
import org.scribble.f17.ast.local.F17Projector;
import org.scribble.main.Job;
import org.scribble.main.MainContext;
import org.scribble.main.ScribbleException;
import org.scribble.main.resource.DirectoryResourceLocator;
import org.scribble.main.resource.ResourceLocator;
import org.scribble.sesstype.name.GProtocolName;
import org.scribble.sesstype.name.Role;
import org.scribble.util.ScribParserException;
// import ast.binary.Type;

// f17-specific CL frontend
//
// Args: either
//   -inline "..inline module def.." [proto-name]
//   main-mod-path [proto-name]
public class F17Main
{
	public static void main(String[] args) throws ScribbleException, ScribParserException
	{
		//F17GType g = null;

		String inline = null;
		Path mainpath = null;
		String simpname;
		try
		{
			if (args[0].equals("-inline"))
			{
				inline = args[1];
				simpname = (args.length < 3) ? "Proto" : args[2];  // Looks for protocol named "Proto" as default if unspecified
			}
			else
			{
				mainpath = Paths.get(args[0]);
				simpname = (args.length < 2) ? "Proto" : args[1];
			}
			MainContext mc = F17Main.newMainContext(inline, mainpath);
			parseAndCheckWF(mc.newJob(), new GProtocolName(simpname));
		}
		catch (ScribParserException | ScribbleException e)
		{
			System.err.println(e.getMessage());
			System.exit(1);
		}
		//System.out.println("Translated:\n" + "    " + g);
	}

	// For alternative f17-specific Main
	// Duplicated from CommandLine for convenience
	// Pre: one of inline/mainpath is null
	private static MainContext newMainContext(String inline, Path mainpath) throws ScribParserException, ScribbleException
	{
		boolean debug = false;
		boolean useOldWF = false;
		boolean noLiveness = false;
		boolean minEfsm = false;
		boolean fair = false;
		boolean noLocalChoiceSubjectCheck = false;
		boolean noAcceptCorrelationCheck = true;
		boolean noValidation = true;  // FIXME: deprecate -- redundant due to hardcoded Job.checkLinearMPScalaWellFormedness

		boolean f17 = true;

		/*List<Path> impaths = this.args.containsKey(ArgFlag.PATH)
				? CommandLine.parseImportPaths(this.args.get(ArgFlag.PATH)[0])
				: Collections.emptyList();*/
		List<Path> impaths = Collections.emptyList();  // FIXME: get from Main args
		ResourceLocator locator = new DirectoryResourceLocator(impaths);
		return (inline == null)
				? new MainContext(debug, locator, mainpath, useOldWF, noLiveness, minEfsm, fair,
							noLocalChoiceSubjectCheck, noAcceptCorrelationCheck, noValidation, f17)
				: new MainContext(debug, locator, inline, useOldWF, noLiveness, minEfsm, fair,
							noLocalChoiceSubjectCheck, noAcceptCorrelationCheck, noValidation, f17);
	}

	// Used from above and from cli.CommandLine
	public static F17GType parseAndCheckWF(Job job, GProtocolName simplename) throws ScribbleException, ScribParserException
	{
		job.runContextBuildingPasses();
		
		Module main = job.getContext().getMainModule();
		if (!main.hasProtocolDecl(simplename))
		{
			throw new ScribbleException("Global protocol not found: " + simplename);
		}
		GProtocolDecl gpd = (GProtocolDecl) main.getProtocolDecl(simplename);

		F17GType gt = new F17GProtocolDeclTranslator().translate(job.getContext(), ((ModuleDel) main.del()).getModuleContext(), gpd);

		//job.debugPrintln
		System.out.println("[f17] Translated:\n  " + gt);

		F17Projector p = new F17Projector();
		for (Role r : gpd.header.roledecls.getRoles())
		{
			F17LType lt = p.project(gt, r, Collections.emptySet());

			//job.debugPrintln
			System.out.println("[f17] Projected onto " + r + ":\n  " + lt);
		}
		
		job.runUnfoldingPass();
		job.runWellFormednessPasses();
		
		return gt;
	}
}
