package org.scribble.ext.assrt.core.model.global;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.endpoint.EGraph;
import org.scribble.core.model.global.SGraphBuilderUtil;
import org.scribble.core.model.global.SSingleBuffers;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtFormulaFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtIntVarFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtSmtFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtIntVar;
import org.scribble.ext.assrt.model.endpoint.AssrtEState;
import org.sosy_lab.java_smt.api.Formula;

public class AssrtCoreSGraphBuilderUtil extends SGraphBuilderUtil
{
	protected AssrtCoreSGraphBuilderUtil(ModelFactory mf)
	{
		super(mf);
	}
	
	// TODO: factor out of util, cf. SGraphBuilder.createInitConfig
	@Override
	protected AssrtCoreSConfig createInitConfig(Map<Role, EGraph> egraphs,
			boolean explicit)
	{
		throw new RuntimeException("Deprecated.");
	}

	protected AssrtCoreSConfig createInitConfig(Map<Role, EGraph> egraphs,
			boolean explicit, Map<AssrtIntVar, DataName> Env)
	{
		Map<Role, EFsm> P = egraphs.entrySet().stream()
				.collect(Collectors.toMap(Entry::getKey, e -> e.getValue().toFsm()));
		SSingleBuffers Q = new AssrtCoreSSingleBuffers(P.keySet(), !explicit);  // TODO: refactor queues creation via modelfactory (cf. super)
		return ((AssrtCoreSModelFactory) this.mf.global).AssrtCoreSConfig(P, Q,
				makeK(P.keySet()), makeF(P),
				makeV(P), makeR(P),
				Env  // Should initially just the svar sorts (for all svars, including nested) -- TODO: currently just empty, hacked inside AssrtCoreSConfig for now
				);
	}

	private static Map<Role, Set<AssrtIntVar>> makeK(Set<Role> rs)
	{
		return rs.stream().collect(Collectors.toMap(r -> r, r -> new HashSet<>()));
	}

	//private static Map<Role, Set<AssrtBoolFormula>> makeF(Set<Role> rs)
	private static Map<Role, Set<AssrtBFormula>> makeF(
			Map<Role, EFsm> P)
	{
		//return rs.stream().collect(Collectors.toMap(r -> r, r -> new HashSet<>()));
		return P.entrySet().stream().collect(Collectors.toMap(
				Entry::getKey,
				/*e -> e.getValue().getStateVars().entrySet().stream()
						.map(b -> AssrtFormulaFactory.AssrtBinComp(
								AssrtBinCompFormula.Op.Eq, 
								AssrtFormulaFactory.AssrtIntVar(b.getKey().toString()),
								b.getValue()))
						.collect(Collectors.toSet())*/
				x -> new HashSet<>()));
	}

	/*private static Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> 
			makeScopes(Map<Role, EFsm> P)
	{
		return P.entrySet().stream()
				.collect(Collectors.toMap(Entry::getKey, x -> new LinkedHashMap<>()));
	}*/

	// TODO: EFsm -> EGraph
	private static Map<Role, Map<AssrtIntVar, AssrtAFormula>> makeV(
			Map<Role, EFsm> P)
	{
		Map<Role, Map<AssrtIntVar, AssrtAFormula>> V = P.entrySet()
				.stream().collect(Collectors.toMap(
						Entry::getKey,
						e -> new HashMap<>(
								//((AssrtEState) e.getValue().graph.init).getStateVars() // Deprecating special case treatment of statevar init exprs and "constants"
								((AssrtEState) e.getValue().graph.init).getStateVars()
										.entrySet().stream().collect(Collectors.toMap(
												Entry::getKey,
												//x -> renameIntVarAsFormula(x.getKey())
												Entry::getValue
												)))));
		/*Map<Role, Map<AssrtDataTypeVar, AssrtArithFormula>> R = P.keySet().stream().collect(Collectors.toMap(r -> r, r ->
				Stream.of(false).collect(Collectors.toMap(
						x -> AssrtCoreESend.DUMMY_VAR,
						x -> AssrtCoreESend.ZERO))
			));*/
		for (Role r : P.keySet())
		{
			Map<AssrtIntVar, AssrtAFormula> tmp = V.get(r);
			for (Entry<AssrtIntVar, AssrtAFormula> e : ((AssrtEState) P
					.get(r).graph.init).getPhantoms().entrySet())
			{
				tmp.put(e.getKey(), e.getValue());
			}
		}

		return V;
	}

	private static Map<Role, Set<AssrtBFormula>> makeR(Map<Role, EFsm> P)
	{
		return P.entrySet().stream().collect(Collectors.toMap(
				Entry::getKey,
				x ->
				{
					Set<AssrtBFormula> set = new HashSet<>();
						AssrtBFormula ass = ((AssrtEState) x.getValue().graph.init)
								.getAssertion();
						if (!ass.equals(AssrtTrueFormula.TRUE))
					{
						set.add(ass);
					}
					return set;
				}
		));
	}

	/* Static helpers */

	public static <T extends Formula> AssrtSmtFormula<T> renameFormula(
			AssrtSmtFormula<T> f)
	{
		for (AssrtIntVar v : f.getIntVars())
		{
			AssrtIntVarFormula old = AssrtFormulaFactory.AssrtIntVar(v.toString());  // N.B. making *Formula*
			AssrtIntVarFormula fresh = AssrtFormulaFactory
					.AssrtIntVar("_" + v.toString());  // FIXME HACK
			f = f.subs(old, fresh);  // N.B., works on Formulas
		}
		return f;
	}

	// "x" -> "_x" -- IntVar is a name, translating to a "fresh" formula
	public static AssrtIntVarFormula renameIntVarAsFormula(AssrtIntVar svar)
	{
		return (AssrtIntVarFormula) renameFormula(  // Adds "_" prefix
				AssrtFormulaFactory.AssrtIntVar(svar.toString()));
	}
}
