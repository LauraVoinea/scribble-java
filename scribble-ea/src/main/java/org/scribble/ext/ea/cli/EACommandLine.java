package org.scribble.ext.ea.cli;

import org.scribble.cli.CommandLine;
import org.scribble.cli.CommandLineException;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.config.*;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.AntlrSourceException;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;

// Includes assrt-core functionality (all extra args are currently for assrt-core)
public class EACommandLine extends CommandLine
{
	public EACommandLine(String... args)
	{
		super(args);
	}

	public static void main(String[] args)
			throws CommandLineException, AntlrSourceException
	{
		/*%%   p (+) { l1(A) -> p & { ... }, l2(B) -> p (+) { l3(C) -> End } }
%%   let () <=
%%       if rand () then
%%           p ! l1(V);
%%           suspend h1 -- p (+) { l3(C) -> End }
%%       else
%%           p ! l2(W) -- p (+) { l3(C) -> End }
%%   in
%%   -- p (+) { l3(C) -> End }
%%   print("still alive")*/

		EAPRuntimeFactory rf = EAPRuntimeFactory.factory;
		EAPFactory f = EAPFactory.factory;

		//ex1(rf, f);
		ex2(rf, f);

		//new EACommandLine(args).run();
	}

	private static void ex2(EAPRuntimeFactory rf, EAPFactory f) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		Op l2 = new Op("l2");
		EAPUnit unit = f.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");
		EAPVar x = f.var("x");

		EAPSend sendAB1 = f.send(B, l1, unit);
		EAPSend sendAB2 = f.send(B, l2, unit);

		EAPLet let = f.let(x, sendAB1, sendAB2);

		EAPActiveThread tA = rf.activeThread(let, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		LinkedHashMap<Op, Pair<EAPVar, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPIdle idle = rf.idle();
		Hs.put(l2, new EAPPair<EAPVar, EAPExpr>(x, f.returnn(f.unit())));
		EAPHandlers hB2 = f.handlers(B, Hs);

		Hs = new LinkedHashMap<>();
		EAPReturn ret = f.returnn(hB2);  // XXX register
		Hs.put(l1, new EAPPair<EAPVar, EAPExpr>(x, ret));
		EAPHandlers hB1 = f.handlers(B, Hs);

		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hB1);
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		System.out.println(cA);
		System.out.println(cB);

		LinkedHashMap<EAPPid, EAPConfig> cs = new LinkedHashMap<>();
		cs.put(p1, cA);
		cs.put(p2, cB);
		EAPSystem sys = rf.system(cs);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);

	}

	private static void ex1(EAPRuntimeFactory rf, EAPFactory f) {
		Role A = new Role("A");
		Role B = new Role("B");
		Op l1 = new Op("l1");
		EAPUnit unit = f.unit();
		EAPSid s = rf.sid("s");
		EAPPid p1 = rf.pid("p1");
		EAPPid p2 = rf.pid("p2");

		EAPSend sendAB = f.send(B, l1, unit);
		EAPActiveThread tA = rf.activeThread(sendAB, s, A);
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaA = new LinkedHashMap<>();
		EAPConfig cA = rf.config(p1, tA, sigmaA);

		LinkedHashMap<Op, Pair<EAPVar, EAPExpr>> Hs = new LinkedHashMap<>();
		EAPVar x = f.var("x");
		EAPReturn ret = f.returnn(unit);
		Hs.put(l1, new EAPPair<EAPVar, EAPExpr>(x, ret));
		EAPHandlers hB = f.handlers(B, Hs);
		EAPIdle idle = rf.idle();
		LinkedHashMap<Pair<EAPSid, Role>, EAPHandlers> sigmaB = new LinkedHashMap<>();
		sigmaB.put(new EAPPair<>(s, B), hB);
		EAPConfig cB = rf.config(p2, idle, sigmaB);

		System.out.println(cA);
		System.out.println(cB);

		LinkedHashMap<EAPPid, EAPConfig> cs = new LinkedHashMap<>();
		cs.put(p1, cA);
		cs.put(p2, cB);
		EAPSystem sys = rf.system(cs);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p1);
		System.out.println();
		System.out.println(sys);

		sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);

		/*sys = sys.reduce(p2);
		System.out.println();
		System.out.println(sys);*/
	}


























	/*@Override
	protected AssrtCLFlags newCLFlags()
	{
		return new AssrtCLFlags();
	}*/
	
	/*// Based on CommandLine.newMainContext
	@Override
	protected Main newMain() throws ScribParserException, ScribException
	{
		if (!hasFlag(AssrtCLFlags.ASSRT_CORE_FLAG))
		{
			return super.newMain();
		}
		AssrtCoreArgs args = newCoreArgs();
		List<Path> impaths = parseImportPaths();
		ResourceLocator locator = new DirectoryResourceLocator(impaths);
		Path mainpath = parseMainPath();
		return new AssrtMain(locator, mainpath, args);
	}*/

	/*@Override
	protected AssrtCoreArgs newCoreArgs()
	{
		Solver solver = hasFlag(AssrtCLFlags.ASSRT_CORE_NATIVE_Z3_FLAG)
				? AssrtJob.Solver.NATIVE_Z3
				: AssrtJob.Solver.NONE;  // TODO: 
		boolean z3Batch = hasFlag(AssrtCLFlags.ASSRT_CORE_BATCH_Z3_FLAG);
		Set<CoreFlags> flags = parseCoreFlags();
		return new AssrtCoreArgs(flags, solver, z3Batch);
	}*/

	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	// TODO: refactor into AssrtCore as needed
	
/*
	@Override
	protected void runValidationTasks(Job job) throws AssrtCoreSyntaxException,
			AntlrSourceException, ScribParserException, CommandLineException
	{
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE))  // assrt-*core* mode
		{

			doAssrtCoreValidationTasks((AssrtJob) job);
		}
		else
		{
			super.runValidationTasks(job);
		}
	}

	private void doAssrtCoreValidationTasks(AssrtJob j)
			throws AssrtCoreSyntaxException, ScribException, ScribParserException,
			CommandLineException
	{
//		if (this.args.containsKey(CLArgFlag.PROJECT))  // HACK
//			// modules/f17/src/test/scrib/demo/fase17/AppD.scr in [default] mode bug --- projection/EFSM not properly formed if this if is commented ????
//		{
//
//		}

		assrtCorePreContextBuilding(j);

		GProtoName simpname = new GProtoName(this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_CORE)[0]);
		if (simpname.toString().equals("[AssrtCoreAllTest]"))  // HACK: AssrtCoreAllTest
		{
			assrtCoreParseAndCheckWF(j);  // Includes base passes
		}
		else
		{
			assrtCoreParseAndCheckWF(j, simpname);  // Includes base passes
		}
		
		// FIXME? assrt-core FSM building only used for assrt-core validation -- output tasks, e.g., -api, will still use default Scribble FSMs
		// -- but the FSMs should be the same? -- no: action assertions treated differently in core than base
	}

	
	// Refactor into Assrt(Core)Job?
	// Following methods are for assrt-*core*
	
	private void assrtCorePreContextBuilding(AssrtJob job) throws ScribException
	{
		job.runContextBuildingPasses();

		//job.runVisitorPassOnParsedModules(RecRemover.class);  // FIXME: Integrate into main passes?  Do before unfolding? 
				// FIXME: no -- revise to support annots
	}

	// Pre: assrtPreContextBuilding(job)
	private void assrtCoreParseAndCheckWF(AssrtJob job)
			throws AssrtCoreSyntaxException, ScribException, ScribParserException,
			CommandLineException
	{
		Module main = job.getContext().getMainModule();
		for (GProtoDecl gpd : main.getGlobalProtocolDecls())
		{
			if (!gpd.isAuxModifier())
			{
				assrtCoreParseAndCheckWF(job, gpd.getHeader().getDeclName());  // decl name is simple name
			}
		}
	}

	// Pre: assrtPreContextBuilding(job)
	private void assrtCoreParseAndCheckWF(AssrtJob job, GProtoName simpname)
			throws AssrtCoreSyntaxException, ScribException, ScribParserException,
			CommandLineException
	{
		Module main = job.getContext().getMainModule();
		if (!main.hasProtocolDecl(simpname))
		{
			throw new AssrtException("[assrt-core] Global protocol not found: " + simpname);
		}
		this.gpd = (GProtoDecl) main.getProtocolDecl(simpname);

		AssrtCoreSTypeFactory af = new AssrtCoreSTypeFactory();
		AssrtCoreGType gt = new AssrtCoreGProtocolDeclTranslator(job, af).translate(this.gpd);
		
//		..HERE FIXME: need to add global assrt rec/continue and fix global inlining -- below steps use only the inlined *global*
//		CHECKME: base assrt "works" because projected local proto decl does keep the assertion, and inlining of local, which does handle the assertions (AssrtLProjectionDeclDel), is done from the "base" protocol decl(s) -- i.e., not from the inlined global (CHECKME?)
//		(in base, inlining of global is only for global level (syntactic) checks -- model checking is done from the separately inlined locals -- inlined global is also for "extensions" like this one and f17)
//		-- does inlining->projection give the same as "base projection"->inlining?
		
		job.verbosePrintln("\n[assrt-core] Translated:\n  " + gt);
		
		List<AssrtDataTypeVar> adts = gt.collectAnnotDataTypeVarDecls().stream()
				.map(v -> v.var).collect(Collectors.toList());
		job.verbosePrintln(
				"\n[assrt-core] Collected data type annotation var decls: " + adts);
		Set<AssrtDataTypeVar> dups = adts.stream()
				.filter(i -> Collections.frequency(adts, i) > 1)
				.collect(Collectors.toSet());
		if (dups.size() > 0)
		{
			throw new AssrtCoreSyntaxException(
					"[assrt-core] Repeat data type annotation variable declarations not allowed: "
							+ dups);
		}

		for (Role r : gpd.header.roledecls.getRoles())
		{
			AssrtCoreLType lt = gt.project(af, r, AssrtTrueFormula.TRUE);
			P0.put(r, lt);

			job.verbosePrintln("\n[assrt-core] Projected onto " + r + ":\n  " + lt);
		}

		AssrtCoreEGraphBuilder builder = new AssrtCoreEGraphBuilder(job);
		this.E0 = new HashMap<>();
		for (Role r : P0.keySet())
		{
			EGraph g = builder.build(P0.get(r));
			this.E0.put(r, (AssrtEState) g.init);

			
			job.verbosePrintln("\n[assrt-core] Built endpoint graph for " + r + ":\n" + g.toDot());
		}
				
		assrtCoreValidate(job, simpname, gpd.isExplicitModifier());//, this.E0);  // TODO

//		if (!job.fair)
//		{
//			Map<Role, EState> U0 = new HashMap<>();
//			for (Role r : E0.keySet())
//			{
//				EState u = E0.get(r).unfairTransform();
//				U0.put(r, u);
//
//				job.verbosePrintln
//				//System.out.println
//						("\n[assrt-core] Unfair transform for " + r + ":\n" + u.toDot());
//			}
//			
//			//validate(job, gpd.isExplicitModifier(), U0, true);  //TODO
//		}
		
		//((AssrtJob) job).runF17ProjectionPasses();  // projections not built on demand; cf. models

		//return gt;
	}
		
	// HACK: store in (Core) Job/JobContext?
	protected GProtoDecl gpd;
	protected Map<Role, AssrtCoreLType> P0 = new HashMap<>();
	protected Map<Role, AssrtEState> E0;  // There is no core version
	protected AssrtCoreSModel model;

	// FIXME: factor out -- cf. super.doAttemptableOutputTasks
	@Override
	protected void tryOutputTasks(Job job) throws CommandLineException, ScribException
	{
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE_PROJECT))
		{
			String[] args = this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_CORE_PROJECT);
			for (int i = 0; i < args.length; i += 1)
			{
				Role role = CommandLine.checkRoleArg(job.getContext(), gpd.getHeader().getDeclName(), args[i]);
				String out = P0.get(role).toString();
				System.out.println("\n" + out);  // Endpoint graphs are "inlined" (a single graph is built)
			}
		}
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE_EFSM))
		{
			String[] args = this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_CORE_EFSM);
			for (int i = 0; i < args.length; i += 1)
			{
				Role role = CommandLine.checkRoleArg(job.getContext(), gpd.getHeader().getDeclName(), args[i]);
				String out = E0.get(role).toDot();
				System.out.println("\n" + out);  // Endpoint graphs are "inlined" (a single graph is built)
			}
		}
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE_EFSM_PNG))
		{
			String[] args = this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_CORE_EFSM_PNG);
			for (int i = 0; i < args.length; i += 2)
			{
				Role role = CommandLine.checkRoleArg(job.getContext(), gpd.getHeader().getDeclName(), args[i]);
				String png = args[i+1];
				String out = E0.get(role).toDot();
				runDot(out, png);
			}
		}
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE_MODEL))
		{
			System.out.println("\n" + model.toDot());
		}
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE_MODEL_PNG))
		{
			String[] arg = this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_CORE_MODEL_PNG);
			String png = arg[0];
			runDot(model.toDot(), png);
		}

		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_STP_EFSM))
		{
			String[] args = this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_STP_EFSM);
			for (int i = 0; i < args.length; i += 1)
			{
				Role role = CommandLine.checkRoleArg(job.getContext(), gpd.getHeader().getDeclName(), args[i]);
				AssrtStpEState init = AssrtStpEState.from((AssrtCoreEModelFactory) job.ef, E0.get(role));
				String out = init.toDot();
				System.out.println("\n" + out);  // Endpoint graphs are "inlined" (a single graph is built)
			}
		}
		if (this.assrtCoreArgs.containsKey(AssrtCoreCLFlags.ASSRT_CORE_EFSM_PNG))
		{
			String[] args = this.assrtCoreArgs.get(AssrtCoreCLFlags.ASSRT_CORE_EFSM_PNG);
			for (int i = 0; i < args.length; i += 2)
			{
				Role role = CommandLine.checkRoleArg(job.getContext(), gpd.getHeader().getDeclName(), args[i]);
				String png = args[i+1];
				AssrtStpEState init = AssrtStpEState.from((AssrtCoreEModelFactory) job.ef, E0.get(role));
				String out = init.toDot();
				runDot(out, png);
			}
		}
	}

	private void assrtCoreValidate(Job job, GProtoName simpname, boolean isExplicit, 
			//Map<Role, AssrtEState> E0,
			boolean... unfair) throws ScribException, CommandLineException
	{
		this.model = new AssrtCoreSModelBuilder(job.sf).build(this.E0, isExplicit);

		job.verbosePrintln("\n[assrt-core] Built model:\n" + this.model.toDot());
		
		if (unfair.length == 0 || !unfair[0])
		{
			AssrtCoreSafetyErrors serrs = this.model.getSafetyErrors(job, simpname);  // job just for debug printing
			if (serrs.isSafe())
			{
				job.verbosePrintln("\n[assrt-core] Protocol safe.");
			}
			else
			{
				throw new AssrtException("[assrt-core] Protocol not safe:\n" + serrs);
			}
		}
		
//		F17ProgressErrors perrs = m.getProgressErrors();
//		if (perrs.satisfiesProgress())
//		{
//			job.verbosePrintln
//			//System.out.println
//					("\n[f17] " + ((unfair.length == 0) ? "Fair protocol" : "Protocol") + " satisfies progress.");
//		}
//		else
//		{
//
//			// FIXME: refactor eventual reception as 1-bounded stable check
//			Set<F17SState> staberrs = m.getStableErrors();
//			if (perrs.eventualReception.isEmpty())
//			{
//				if (!staberrs.isEmpty())
//				{
//					throw new RuntimeException("[f17] 1-stable check failure: " + staberrs);
//				}
//			}
//			else
//			{
//				if (staberrs.isEmpty())
//				{
//					throw new RuntimeException("[f17] 1-stable check failure: " + perrs);
//				}
//			}
//			
//			throw new F17Exception("\n[f17] " + ((unfair.length == 0) ? "Fair protocol" : "Protocol") + " violates progress.\n" + perrs);
//		}
	}
	*/
}
