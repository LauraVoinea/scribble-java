/**
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.scribble.ext.assrt.core.job;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.scribble.core.job.Core;
import org.scribble.core.job.CoreContext;
import org.scribble.core.lang.ProtoMod;
import org.scribble.core.lang.global.GProtocol;
import org.scribble.core.lang.local.LProjection;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.global.SGraph;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.*;
import org.scribble.core.visit.STypeVisitorFactory;
import org.scribble.core.visit.STypeVisitorFactoryImpl;
import org.scribble.core.visit.global.GTypeVisitorFactoryImpl;
import org.scribble.ext.assrt.core.lang.global.AssrtGProtocol;
import org.scribble.ext.assrt.core.model.endpoint.AssrtEModelFactoryImpl;
import org.scribble.ext.assrt.core.model.formal.endpoint.RCA;
import org.scribble.ext.assrt.core.model.formal.endpoint.RCAState;
import org.scribble.ext.assrt.core.model.global.AssrtSGraph;
import org.scribble.ext.assrt.core.model.global.AssrtSModelFactory;
import org.scribble.ext.assrt.core.model.global.AssrtSModelFactoryImpl;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formal.global.AssrtFormalGTranslator;
import org.scribble.ext.assrt.core.type.formal.global.AssrtFormalGlobal;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLocal;
import org.scribble.ext.assrt.core.type.formal.local.AssrtLambda;
import org.scribble.ext.assrt.core.type.formal.local.AssrtRho;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtSTypeFactory;
import org.scribble.ext.assrt.core.type.session.global.AssrtGTypeFactory;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGConfig;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGEnv;
import org.scribble.ext.assrt.core.visit.gather.AssrtRoleGatherer;
import org.scribble.ext.assrt.core.visit.local.AssrtLTypeVisitorFactoryImpl;
import org.scribble.ext.assrt.job.AssrtJob.Solver;
import org.scribble.ext.assrt.util.AssrtUtil;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.ext.assrt.util.Z3Wrapper;
import org.scribble.util.Pair;
import org.scribble.util.ScribException;


// A "compiler job" front-end that supports operations comprising visitor passes over the AST and/or local/global models
public class AssrtCore extends Core
{
	public AssrtCore(ModuleName mainFullname, AssrtCoreArgs args,
					 Set<GProtocol> imeds, AssrtSTypeFactory tf)
	{
		super(mainFullname, args, imeds, tf);
	}
	
	// A Scribble extension should override newSTypeVisitorFactory/ModelFactory as appropriate
	@Override
	protected STypeVisitorFactory newSTypeVisitorFactory()
	{
		return new STypeVisitorFactoryImpl(new GTypeVisitorFactoryImpl(),
				new AssrtLTypeVisitorFactoryImpl());
	}
	
	// A Scribble extension should override newSTypeVisitorFactory/ModelFactory as appropriate
	@Override
	protected ModelFactory newModelFactory()
	{
		return new ModelFactory(
				(Function<ModelFactory, EModelFactory>) AssrtEModelFactoryImpl::new,  // Explicit cast necessary (CHECKME, why?)
				(Function<ModelFactory, SModelFactory>) AssrtSModelFactoryImpl::new);
	}

	/*// A Scribble extension should override newCoreConfig/Context/etc as appropriate
	@Override
	protected CoreConfig newCoreConfig(ModuleName mainFullname,
			CoreArgs args, STypeFactory tf)
	{
		STypeVisitorFactory vf = newSTypeVisitorFactory();
		ModelFactory mf = newModelFactory();
		return new CoreConfig(mainFullname, args, tf, vf, mf); 
	}*/

	// A Scribble extension should override newCoreConfig/Context/etc as appropriate
	@Override
	protected CoreContext newCoreContext(Set<GProtocol> imeds)
	{
		return new AssrtCoreContext(this, imeds);
	}

	@Override
	public void runPasses() throws ScribException {
		runSyntaxTransformPasses();
		runGlobalSyntaxWfPasses();  // TODO: consider WF problems that prevent inlining above (e.g., distinct annot vars, AssrtCoreContextget.Inlined)
		runProjectionPasses();  // CHECKME: can try before validation (i.e., including syntactic WF), to promote greater tool feedback? (cf. CommandLine output "barrier")
		//runProjectionSyntaxWfPasses();
		runEfsmBuildingPasses();  // Currently, unfair-transform graph building must come after syntactic WF --- TODO fix graph building to prevent crash ?
		runLocalModelCheckingPasses();
		runGlobalModelCheckingPasses();

		//tempRunSyncSat();  // XXX HERE global model building
		foo();
	}

	private void foo() {
		System.out.println("\n--------------------\n");
		AssrtFormalLFactory lf = AssrtFormalLFactory.factory;
		AssrtFormalGTranslator tr = new AssrtFormalGTranslator();
		AssrtCoreContext c = (AssrtCoreContext) this.context;
		Map<ProtoName<Global>, GProtocol> inlined = c.getInlined();
		for (Map.Entry<ProtoName<Global>, GProtocol> e : inlined.entrySet()) {
			AssrtGProtocol g = (AssrtGProtocol) e.getValue();
			if (!g.mods.contains(ProtoMod.AUX)) {
				AssrtFormalGlobal g1 = tr.translate(g.type);
				System.out.println("aaa: " + g.fullname + " ,, " + g1);
				Set<Role> rs = g.type.assrtCoreGather(new AssrtRoleGatherer()::visit).collect(Collectors.toSet());
				for (Role r : rs) {
					AssrtFormalLocal p = g1.project(lf, r);
					System.out.println("\nbbb: " + r + " ,, " + p);

					AssrtLambda lam = new AssrtLambda();
					AssrtRho rho = new AssrtRho();
					//stepper(lam, p);

					Map<Pair<AssrtLambda, AssrtFormalLocal>, Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>>> graph = new LinkedHashMap<>();

					dstepper(lam, rho, p, graph);  //HERE make finite graph and do RCA translation

					for (Map.Entry<Pair<AssrtLambda, AssrtFormalLocal>, Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>>> f : graph.entrySet()) {
						Pair<AssrtLambda, AssrtFormalLocal> k = f.getKey();
						Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>> v = f.getValue();
						System.out.println(AssrtUtil.pairToString(k) + " ,, " +
								v.entrySet().stream().map(x -> x.getKey() + " -> " + AssrtUtil.pairToString(x.getValue())).collect(Collectors.joining(" ,, ")));
					}

					RCA rca = new RCA();
					rca(graph, new Pair<>(lam, p), null, null, RCAState.fresh(), rca);

					System.out.println("eee: " + rca);
				}
			}
		}
	}

	private void stepper(AssrtLambda lam, AssrtFormalLocal t) {
		System.out.println("ccc: " + lam + " ,, " + t);
		Set<AssrtLAction> steppable = t.getSteppable(lam);
		for (AssrtLAction a : steppable) {
			System.out.println("ddd: " + lam + " ,, " + t + " ,, " + a);
			Optional<Pair<AssrtLambda, AssrtFormalLocal>> step = t.step(lam, a);
			if (!step.isPresent()) {
				throw new RuntimeException("FIXME ");
			}
			Pair<AssrtLambda, AssrtFormalLocal> res = step.get();
			stepper(res.left, res.right);
		}
	}

	private void dstepper(AssrtLambda lam, AssrtRho rho, AssrtFormalLocal t,
			  //Map<Pair<Pair<AssrtLambda, AssrtFormalLocal>, AssrtLAction>, Pair<AssrtLambda, AssrtFormalLocal>> graph) {
				Map<Pair<AssrtLambda, AssrtFormalLocal>, Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>>> graph) {
		System.out.println("ccc1: " + lam + " ,, " + t);
		Set<AssrtLAction> dsteppable = t.getDerivSteppable(lam, rho);

		Pair<AssrtLambda, AssrtFormalLocal> k = new Pair<>(lam, t);
		Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>> as = graph.get(k);
		if (as == null) {
			as = new HashMap<>();
			graph.put(k, as);
		}

		for (AssrtLAction a : dsteppable) {
			System.out.println("ddd1: " + lam + " ,, " + t + " ,, " + a);
			Optional<Triple<AssrtLambda, AssrtFormalLocal, AssrtRho>> step = t.dstep(lam, rho, a);
			if (!step.isPresent()) {
				throw new RuntimeException("FIXME ");
			}
			Triple<AssrtLambda, AssrtFormalLocal, AssrtRho> res = step.get();

			as.put(a, new Pair<>(res.left, res.middle));

			dstepper(res.left, res.right, res.middle, graph);
		}
	}

	// Pre: s1 != null => res.S.contains(s1) -- and n corresponds to s2
	private void rca(Map<Pair<AssrtLambda, AssrtFormalLocal>, Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>>> graph,
					Pair<AssrtLambda, AssrtFormalLocal> n,
					RCAState s1, AssrtLAction a, RCAState s2, RCA res) {
		if (res.S.contains(s2)) {
			return;
		}
		RCAState fresh = RCAState.fresh();
		Map<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>> as = graph.get(n);
		for (Map.Entry<AssrtLAction, Pair<AssrtLambda, AssrtFormalLocal>> e : as.entrySet()) {

			Pair<AssrtLambda, AssrtFormalLocal> succ = e.getValue();
			rca(graph, succ, s2, e.getKey(), fresh, res);

		}
		res.S.add(s2);
		if (s1 != null) {
			res.delta.put(s1, new Pair<>(a, s2));
		}
		res.sigma.put(s2, n.left);
	}

	private void tempRunSyncSat() throws ScribException {
		System.out.println("\n--------------------\n");
		AssrtCoreContext c = (AssrtCoreContext) this.context;
		Map<ProtoName<Global>, GProtocol> inlined = c.getInlined();
		for (Map.Entry<ProtoName<Global>, GProtocol> e : inlined.entrySet()) {
			AssrtGProtocol g = (AssrtGProtocol) e.getValue();
			if (!g.mods.contains(ProtoMod.AUX)) {
				tempRunSyncSat(g);
			}
		}
	}

	private void tempRunSyncSat(AssrtGProtocol g) throws ScribException {
		AssrtSModelFactory sf = (AssrtSModelFactory) this.config.mf.global;
		AssrtGTypeFactory gf = (AssrtGTypeFactory) this.config.tf.global;

		// Cf. SState -- needs SConfig which is coupled to EFSMs and queues (i.e., async)
		Map<AssrtGConfig, Map<AssrtSSend, AssrtGConfig>> graph = new HashMap<>();

		Set<Pair<AssrtGConfig, AssrtSSend>> done = new HashSet<>();
		Set<Pair<AssrtGConfig, AssrtSSend>> todo = new HashSet<>();
		AssrtGConfig init = new AssrtGConfig(
				new AssrtGEnv(Collections.EMPTY_MAP, Collections.EMPTY_MAP, AssrtTrueFormula.TRUE),
				g.type);
		Map<Role, Set<AssrtSSend>> actions
				= g.type.collectImmediateActions(sf, Collections.emptyMap());
		for (Map.Entry<Role, Set<AssrtSSend>> a : actions.entrySet()) {
			a.getValue().forEach(x -> todo.add(new Pair<>(init, x)));
			graph.put(init, new HashMap<>());
		}

		while (!todo.isEmpty()) {
			Pair<AssrtGConfig, AssrtSSend> next = todo.iterator().next();
			todo.remove(next);
			done.add(next);
			if (!graph.containsKey(next.left.type)) {
				graph.put(next.left, new HashMap<>());
			}
			Optional<AssrtGConfig> step = next.left.type.step(gf, next.left.gamma, next.right);
			if (step.isPresent()) {
				AssrtGConfig succ = step.get();
				System.out.println("aaaa: " + next.left.type + " ,, " + next.right + "\n  " + next.left.gamma + "\n  " + succ.type + "\n  " + succ.gamma);
				Map<AssrtSSend, AssrtGConfig> edges = graph.get(next.left);
				edges.put(next.right, succ);
				Map<Role, Set<AssrtSSend>> as
						= succ.type.collectImmediateActions(sf, Collections.emptyMap());
				for (Map.Entry<Role, Set<AssrtSSend>> bs : as.entrySet()) {
					for (AssrtSSend b : bs.getValue()) {
						Pair<AssrtGConfig, AssrtSSend> p = new Pair<>(succ, b);
						if (!done.contains(p)) {
							todo.add(p);
						}
					}
				}
			}
		}

		System.out.println();
		graph.entrySet().forEach(x -> System.out.println(x.getKey() + " ,, " + x.getValue()));


	}

	private static AssrtBFormula assrtprog(AssrtGConfig n, Map<AssrtSSend, AssrtGConfig> edges) {
		//n.gamma.
		return null;
	}

	// knowledge is: all vars in p/q
	
	@Override
	protected void runSyntaxTransformPasses()  // No ScribException, no errors expected
	{
		// More like WF (cf. runGlobalSyntaxWfPasses), but doing before inlining to visit Do's directly
		verbosePrintPass("Checking do argument arities...");
		for (ProtoName<Global> fullname : this.context.getParsedFullnames())
		{
			AssrtGProtocol proto = (AssrtGProtocol) this.context
					.getIntermediate(fullname);
			proto.type.checkDoArgs(this);
		}

		verbosePrintPass("Inlining subprotocols for all globals...");
		for (ProtoName<Global> fullname : this.context.getParsedFullnames())
		{
			GProtocol inlined = this.context.getInlined(fullname);
			verbosePrintPass(
					"Inlined subprotocols: " + fullname + "\n" + inlined);
		}
				
		// Skipping unfolding -- unnecessary with proper guarding
	}

	@Override
	protected void runGlobalSyntaxWfPasses() throws ScribException
	{
		// super.runGlobalSyntaxWfPasses();
		// ^TODO: base API currently not compatible
		// E.g., `this.context.getInlined(fullname).def` is null
		
		verbosePrintPass(
				"Checking for distinct annot vars in each inlined global...");
		// CHECKME: necessary? -- goes against unfolding, duplicates should be allowed in such contexts?
		for (ProtoName<Global> fullname : this.context.getParsedFullnames())
		{
			/*List<AssrtIntVar> vs = ((AssrtCoreGProtocol)
				this.context.getInlined(fullname)).type
					.assrtCoreGather(  // TODO: factor out with base gatherer
							new AssrtCoreIntVarGatherer<Global, AssrtCoreGType>()::visit)
					.collect(Collectors.toList());*/
			AssrtGProtocol proto = (AssrtGProtocol) this.context
					.getInlined(fullname);
			Map<AssrtVar, DataName> svars = new HashMap<>();
			proto.statevars.entrySet()
					.forEach(x -> svars.put(x.getKey(), x.getValue().getSort(svars)));
			List<AssrtVar> vs = proto.type.collectAnnotDataVarDecls(svars).stream()
							.map(x -> x.var).collect(Collectors.toList());
			Set<AssrtVar> distinct = new HashSet<>(vs);
			if (vs.size() != distinct.size())
			{
				throw new ScribException("Duplicate annot var name(s): " + vs);
			}
		}
	}
	
	@Override
	protected void runProjectionPasses()  // No ScribException, no errors expected
	{
		verbosePrintPass("Projecting all inlined globals...");
		for (ProtoName<Global> fullname : this.context.getParsedFullnames())
		{
			GProtocol inlined = this.context.getInlined(fullname);
			for (Role self : inlined.roles)
			{
				// pruneRecs already done (see runContextBuildingPasses)
				// CHECKME: projection and inling commutative?
				LProjection iproj = this.context.getProjectedInlined(inlined.fullname,
						self);
				verbosePrintPass("Projected inlined onto " + self + ": "
						+ inlined.fullname + "\n" + iproj);
			}
		}
		
		// Skipping imed projection
	}

	// Overriding only for a single line, the `validate` call
	@Override
	protected void validateByScribble(ProtoName<Global> fullname, boolean fair)
			throws ScribException
	{
		SGraph graph = fair
				? this.context.getSGraph(fullname)
				: this.context.getUnfairSGraph(fullname);
		if (this.config.args.VERBOSE)
		{
			String dot = graph.init.toDot();
			String[] lines = dot.split("\\R");
			verbosePrintPass(
					//"(" + fullname + ") Built global model...\n" + graph.init.toDot() + "\n(" + fullname + ") ..." + graph.states.size() + " states");
					"Built " + (!fair ? "\"unfair\" " : "") + "global model ("
							+ graph.states.size() + " states): " + fullname + "\n"
							+ ((lines.length > 50)  // CHECKME: factor out constant?
									? "...[snip]...  (model text over 50 lines, try -[u]model[png])"
									: dot));
		}

		verbosePrintPass("Checking " + (!fair ? "\"unfair\" " : "")
				+ "global model: " + fullname);
		((AssrtSModelFactory) this.config.mf.global)
				.AssrtCoreSModel(this, (AssrtSGraph) graph).validate(this);  // FIXME: overriding only for this line (extra core arg)
	}
	
	@Override
	public AssrtCoreContext getContext()
	{
		return (AssrtCoreContext) super.getContext();
	}


	
	
	
	
	
	// Refactor to util? -- also, args (e.g., -z3) to AssrtCoreConfig
	// Maybe record simpname as field (for core)
	public boolean checkSat(GProtoName fullname, Set<AssrtBFormula> bforms)
	{
		Solver solver = ((AssrtCoreArgs) this.config.args).SOLVER;
		AssrtCoreContext corec = getContext();
		switch (solver)
		{
			case NATIVE_Z3:
			{
				return Z3Wrapper.checkSat(this, corec.getIntermediate(fullname), bforms);
			}
			case NONE:
			{
				Map<AssrtVar, DataName> sorts =
						//((AssrtCoreGProtocol) getContext().getInlined(fullname)).type.getBoundSortEnv(Collections.emptyMap());
						((AssrtGProtocol) corec.getInlined(fullname)).getSortEnv();
				verbosePrintln(
						"\n[WARNING] Skipping sat check (did you forget -z3?):\n\t" +
						bforms.stream().map(f -> f.toSmt2Formula(sorts) + "\n\t")
								.collect(Collectors.joining("")));
					return true;
			}
			default:
				throw new RuntimeException(
						"[assrt-core] Shouldn't get in here: " + solver);
		}
	}
}

