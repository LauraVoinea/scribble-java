package org.scribble.ext.assrt.core.model.global;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.stream.Collectors;

import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.model.global.SModel;
import org.scribble.core.model.global.SState;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.job.AssrtCoreArgs;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtEAction;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;

// 1-bounded LTS
// Factor out with SGraph/SModel?
public class AssrtSModel extends SModel
{
	protected final AssrtCore core;
	
	protected AssrtSModel(AssrtCore core, AssrtSGraph graph)
	{
		super(graph);
		this.core = core;
	}
	
	// Maybe refactor simpname (root proto) into the (AssrtCore)Job
	@Override
	protected SortedMap<Integer, AssrtSStateErrors> getSafetyErrors()  // s.id key lighter than full SConfig
	{
		boolean batch = ((AssrtCoreArgs) this.core.config.args).Z3_BATCH;
		boolean batchSat = false;
		
		if (batch)
		{
			GProtoName fullname = this.graph.proto;
			Collection<SState> all = this.graph.states.values();

			if (all.stream().allMatch(x -> ((AssrtSConfig) x.config)
					.getUnknownDataVarErrors(core, fullname).isEmpty()))
			{
				// Check for all errors in a single pass -- any errors can be categorised later
				Set<AssrtBFormula> fs = new HashSet<>();
				fs.addAll(
						all.stream()
								.flatMap(s -> ((AssrtSConfig) s.config)
										.getAssertProgressChecks(this.core, fullname)
										.stream())
								.collect(Collectors.toSet()));
				fs.addAll(
						all.stream()
								.flatMap(s -> ((AssrtSConfig) s.config)
										.getAssertSatChecks(this.core, fullname).stream())
								.collect(Collectors.toSet()));
				fs.addAll(
						((AssrtSConfig) this.graph.init.config)
								.getInitRecAssertChecks(this.core, fullname));
				fs.addAll(
						all.stream().flatMap(s -> ((AssrtSConfig) s.config)
								.getRecAssertChecks(this.core, fullname)
								//s.id == this.graph.init.id)
								.stream())
								.collect(Collectors.toSet()));
				/*String smt2 = fs.stream().filter(f -> !f.equals(AssrtTrueFormula.TRUE))
							.map(f -> "(assert " + f.toSmt2Formula() + ")\n").collect(Collectors.joining(""))
						+ "(check-sat)\n(exit)";
				if (Z3Wrapper.checkSat(smt2))*/  // FIXME: won't work for unint-funs without using Z3Wrapper.toSmt2

				batchSat = this.core.checkSat(fullname, fs);
				if (batchSat)
				{
					return new TreeMap<>();
				}
				/*else
				{
					System.out.println("aaaa: " + fs);
					System.exit(1);
				}*/
			}
		}

		SortedMap<Integer, AssrtSStateErrors> res = new TreeMap<>();
		for (int id : this.graph.states.keySet())
		{
			//SStateErrors errs = this.graph.states.get(id).getErrors();  // TODO: getErrors needs core/fullname args
			AssrtSStateErrors errs = new AssrtSStateErrors(this.core,
					this.graph.proto, (AssrtSState) this.graph.init,
					(AssrtSState) this.graph.states.get(id));
			if (!errs.isEmpty())
			{
				res.put(id, errs);
			}
		}

		// Must come as second pass after initial collection for all states above
		if (!res.isEmpty())
		{
			res = relaxAssrtUnsat(res);
		}

		/*if (batch && !batchSat && res.isEmpty())  // Testing batch vs. base -- now inconvenient due to new assrt-unsat
		{
			throw new RuntimeException("[FIXME] ");
		}*/

		return res;
	}

	// TODO: new assrt-unsat no longer strictly a safety property?
	private SortedMap<Integer, AssrtSStateErrors> relaxAssrtUnsat(
			SortedMap<Integer, AssrtSStateErrors> res)
	{
		SortedMap<Integer, AssrtSStateErrors> tmp = res;
		res = new TreeMap<>();
		for (Entry<Integer, AssrtSStateErrors> e : tmp.entrySet())
		{
			Integer ssid_err = e.getKey();
			AssrtSStateErrors errs = e.getValue();
			AssrtSStateErrors tmp2 = errs;
			for (Entry<Role, Set<AssrtEAction>> e1 : errs.assunsat.entrySet())
			{
				Role subj_unsat = e1.getKey();
				for (AssrtEAction a_unsat : e1.getValue())
				{
					EAction unsat_a1 = (EAction) a_unsat;
					found: for (SState ss_nonerr : (Iterable<SState>) this.graph.states.values()
							.stream().filter(
									x -> !tmp.keySet().contains(x.id))::iterator)  // Subsumes x.id != id -- narrow down to only filter states with unsat errors?
					{
						EFsm efsm = ss_nonerr.config.efsms.get(subj_unsat);
						if (efsm.curr.id == this.graph.states.get(ssid_err).config.efsms
								.get(subj_unsat).curr.id
								&& ss_nonerr.getDetActions().stream()
										.anyMatch(x -> x.subj.equals(subj_unsat)
												&& x.mid.equals(unsat_a1.mid)))
						{
							Map<Role, Set<AssrtEAction>> assunsat = new HashMap<>(
									tmp2.assunsat);
							HashSet<AssrtEAction> tmp3 = new HashSet<>(
									assunsat.get(subj_unsat));
							tmp3.remove(a_unsat);
							if (tmp3.isEmpty())
							{
								assunsat.remove(subj_unsat);
							}
							else
							{
							assunsat.put(subj_unsat, tmp3);
							}
							tmp2 = new AssrtSStateErrors(tmp2, assunsat);
							break found;
						}
					}
				}
			}
			res.put(ssid_err, tmp2);
		}
		SortedMap<Integer, AssrtSStateErrors> tmp4 = res;
		tmp4.keySet().stream().collect(Collectors.toList()).forEach(x ->
			{
				if (tmp4.get(x).isEmpty())
				{
					tmp4.remove(x);
				}
			});
		return res;
	}

	protected Map<SState, Set<ESend>> getAssrtUnsatErrors()
	{
		return null;
	}
}











	
	/*public boolean isActive(AssrtCoreSState s, Role r)
	{
		return AssrtCoreSConfig.isActive(s.getP().get(r), this.E0.get(r).id);
	}
	
	// Revised "eventual reception" -- 1-bounded stable property with subject role side condition
	// FIXME: refactor as actual eventual reception -- though original one may be better for error feedback
	public Set<AssrtCoreSState> getStableErrors()
	{
		Set<AssrtCoreSState> res = new HashSet<>();
		for (AssrtCoreSState s : this.allStates.values())
		{
			if (!AssrtCoreSModel.isStable(s))
			{
				Set<AssrtCoreSState> seen = new HashSet<>();
				if (!canReachStable(seen, s))  // FIXME: subj role side condition for compatibility
				{
					res.add(s);
				}
			}
		}
		return res;
	}
	
	private static boolean isStable(AssrtCoreSState s)  // FIXME: refactor to F17SState
	{
		return s.getQ().values().stream().flatMap(m -> m.values().stream())
				.filter(v -> v != null && !(v instanceof AssrtCoreEBot)).count() == 0;
				// FIXME: connections
	}
	
	//private boolean canReachStable(Set<F17SState> seen, F17SState s, Role r)
	private boolean canReachStable(Set<AssrtCoreSState> seen, AssrtCoreSState s)
			// FIXME: subj role side condition for compatibility -- maybe integrate with this.reach -- currently no point to search this way, should just check stable on all reachable
	{
		if (AssrtCoreSModel.isStable(s))
		{
			return true;
		}
		else if (seen.contains(s))
		{
			return false;
		}
//		for (F17SState succ : s.getAllSuccessors())
//		{
//			if (isStable(succ))
//			{
//				return true;
//			}
//		}
		for (AssrtCoreSState succ : s.getSuccs())
		{
			Set<AssrtCoreSState> tmp = new HashSet<>(seen);
			tmp.add(s);
			if (canReachStable(tmp, succ))
			{
				return true;
			}
		}
		return false;
	}
	*/
