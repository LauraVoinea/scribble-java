package org.scribble.ext.assrt.core.model.global;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.endpoint.EState;
import org.scribble.core.model.endpoint.EStateKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.endpoint.actions.EDisconnect;
import org.scribble.core.model.endpoint.actions.EServerWrap;
import org.scribble.core.model.global.SConfig;
import org.scribble.core.model.global.SSingleBuffers;
import org.scribble.core.type.kind.PayElemKind;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.PayElemType;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.lang.global.AssrtGProtocol;
import org.scribble.ext.assrt.core.lang.local.AssrtLProjection;
import org.scribble.ext.assrt.core.model.endpoint.AssrtEModelFactory;
import org.scribble.ext.assrt.core.model.endpoint.AssrtEState;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtEAction;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtERecv;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtESend;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtAVarFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBinBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBinCompFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtFormulaFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtVarFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLRec;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;

			
public class AssrtSConfig extends SConfig  // TODO: not AssrtSConfig
{
	private static int counter = 1;
	
	// CHECKME: fields used for hash/equals -- cf. SState.config

	// N.B. Shadowing supers for convenience (but at least final and immutable)
	private final Map<Role, EFsm> P;          
	private final SSingleBuffers Q;  // null value means connected and empty -- dest -> src -> msg

	private final Map<AssrtVar, DataName> Env;  // TODO: refactor -- CHECKME: currently only used for payload vars?

	private final Map<Role, Set<AssrtVar>> K;  // Conflict between having this in the state, and formula building?
	private final Map<Role, Set<AssrtBFormula>> F;  // N.B. because F not in equals/hash, "final" receive in a recursion doesn't get built -- cf., unsat check only for send actions
	private final Map<Role, Map<AssrtVar, AssrtAFormula>> V;
	private final Map<Role, Set<AssrtBFormula>> R;  // F is history for action ass's; R is history for rec ass's ?
	// Can action-assertions and state-assertions be positioned as pre/post conditions?  static vs. dynamic enforcement?

	//private final Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename; // combine with K?  // CHECKME: unused?
	
	// *Past* scopes (in the sense of "preceding/outer scope"), so does not include "current" scope -- important to consider for "self-recursions"
	// N.B. not included in equals/hashCode -- used to constrain K/F/etc to syntactic scope to determine the state, but not part of the state itself 
	// (Probably more suitable for the graph builder to manage, but current async/sync methods inconvenient)
	// Reflects lexical scoping -- relies on syntactic WF for var annots
	//protected final Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes;  
			// role -> *EFsm* state id -> past "scope", i.e., known vars up to, but excluding, that state

	// Pre: non-aliased "ownership" of all Map contents
	protected AssrtSConfig(ModelFactory mf, Map<Role, EFsm> P,
			SSingleBuffers Q, Map<Role, Set<AssrtVar>> K,
			Map<Role, Set<AssrtBFormula>> F,
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,

			Map<AssrtVar, DataName> Env

	)
			//Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename
			//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes)
	{
		super(mf, P, Q);
		this.P = this.efsms;  // Already unmodifiable'd (as necessary) by super
		this.Q = this.queues;
		this.K = Collections.unmodifiableMap(K);
		this.F = Collections.unmodifiableMap(F);
		this.V = Collections.unmodifiableMap(V);
		this.R = Collections.unmodifiableMap(R);
		//this.rename = Collections.unmodifiableMap(rename);
		//this.scopes = Collections.unmodifiableMap(scopes);

		this.Env = Collections.unmodifiableMap(Env);

	}

	@Override
	protected Set<EServerWrap> getSWrapFireable(Role self, EFsm fsm)
	{
		throw new RuntimeException("[TODO] : " + fsm + "@" + self);
	}
	
	// Pre: getFireable().get(self).contains(a)
  // Deterministic
	@Override
	public List<SConfig> async(Role self, EAction a)
	//public AssrtCoreSConfig fire(Role self, EAction a)
	{
		List<SConfig> res = new LinkedList<>();
		List<EFsm> succs = this.efsms.get(self).getSuccs(a);
		if (succs.size() > 1)
		{
			throw new RuntimeException(
					"[assrt-core][TODO] Non-deteterministic actions not supported: " + succs);
		}
		for (EFsm succ : succs)
		{
			Map<Role, EFsm> efsms = new HashMap<>(this.efsms);
			efsms.put(self, succ);
			AssrtSConfig next =  // N.B. queue updates are insensitive to non-det "a"
				  a.isSend()       ? fireSend(self, (AssrtESend) a, succ) //this.queues.send(self, (ESend) a)
				: a.isReceive()    ? fireRecv(self, (AssrtERecv) a, succ) //this.queues.receive(self, (ERecv) a)
				//: a.isDisconnect() ? this.queues.disconnect(self, (EDisconnect) a)
				: null;
			if (next == null)
			{
				throw new RuntimeException("Shouldn't get in here: " + a);
			}
			res.add(next);
		}
		return res;
		//R.get(self).putAll(succ.getAnnotVars());  // Should "overwrite" previous var values -- no, do later (and from action info, not state)
	}

	// Update (in place) P, Q, K, F and R
	private AssrtSConfig fireSend(Role self, AssrtESend a, EFsm succ)
	{
		Map<Role, EFsm> P = new HashMap<>(this.P);
		Map<Role, Set<AssrtVar>> K = copyK(this.K);
		Map<Role, Set<AssrtBFormula>> F = copyF(this.F);
		Map<Role, Map<AssrtVar, AssrtAFormula>> V = copyR(this.V);
		//R.get(self).putAll(succ.getAnnotVars());  // Should "overwrite" previous var values -- no, do later (and from action info, not state)
		Map<Role, Set<AssrtBFormula>> R = copyRass(this.R);
		/*Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename = copyRename(
				this.rename);*/
		//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes = copyScopes(this.scopes);

		Map<AssrtVar, DataName> Env = new HashMap<>(this.Env);

		P.put(self, succ);

		AssrtEMsg msg = ((AssrtEModelFactory) this.mf.local).AssrtEMsg(
				a.peer, a.mid, a.payload, a.ass);//, a.sexprs);//, rename.get(self));
		SSingleBuffers Q = this.Q.send(self, msg);

		updateOutput(self, a, succ, K, F, V, R, Env); //rename, scopes);

		return ((AssrtSModelFactory) this.mf.global).AssrtCoreSConfig(P, Q, K,
				F, V, R, Env); //rename scopes, 
	}

  // CHECKME: only need to update self entries of Maps -- almost: except for addAnnotOpensToF, and some renaming via Streams
	private void updateOutput(Role self, AssrtEAction a, EFsm succ,
			Map<Role, Set<AssrtVar>> K, 
			Map<Role, Set<AssrtBFormula>> F,
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,
			Map<AssrtVar, DataName> Env
	)
			//Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename)
			//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes)
	{
		updateKFVR(self, a, a.getAssertion(), succ, K, F, V, R, Env);
	}

	// CHECKME: manage F with receive assertions?
	private AssrtSConfig fireRecv(Role self, AssrtERecv a, EFsm succ)
	{
		Map<Role, EFsm> P = new HashMap<>(this.P);
		Map<Role, Set<AssrtVar>> K = copyK(this.K);
		Map<Role, Set<AssrtBFormula>> F = copyF(this.F);
		Map<Role, Map<AssrtVar, AssrtAFormula>> V = copyR(this.V);
		//R.get(self).putAll(succ.getAnnotVars());  // Should "overwrite" previous var values -- no, do later (and from action info, not state)
		Map<Role, Set<AssrtBFormula>> R = copyRass(this.R);
		/*Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename = copyRename(
				this.rename);*/
		//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes = copyScopes(this.scopes);

		Map<AssrtVar, DataName> Env = new HashMap<>(this.Env);

		P.put(self, succ);
		AssrtEMsg msg = (AssrtEMsg) this.Q.getQueue(self).get(a.peer);  // null is \epsilon
		SSingleBuffers Q = this.Q.receive(self, a);

		updateInput(self, a, msg, //msg.shadow, 
				succ, K, F, V, R,//, rename, scopes
				Env);

		return ((AssrtSModelFactory) this.mf.global).AssrtCoreSConfig(P, Q, K,
				F, V, R,
				//, rename scopes
				Env);
	}

	// "a" is the EFSM input action, which has (hacked) True ass; msg is the dequeued msg, which carries the (actual) ass from the output side
	// CHECKME: factor better with updateOutput ?
	private void updateInput(Role self, AssrtEAction a,
			AssrtEMsg msg, //Map<AssrtIntVarFormula, AssrtIntVarFormula> shadow,
			EFsm succ,
			Map<Role, Set<AssrtVar>> K, Map<Role, Set<AssrtBFormula>> F,
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,
			//Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename  // CHECKME: EAction closest base type -- ?
			//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes
			Map<AssrtVar, DataName> Env)
	{
		updateKFVR(self, a, msg.getAssertion(), succ, K, F, V, R, Env);
	}

	// TODO: pass `Kself`, `Fself`, etc. directly (and not `self`)
	// N.B. `ass` is the difference between output/input (output from a, input from msg), hence a parameter
	private void updateKFVR(Role self, AssrtEAction a, AssrtBFormula ass, EFsm succ,
			Map<Role, Set<AssrtVar>> K, 
			Map<Role, Set<AssrtBFormula>> F,
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,
			Map<AssrtVar, DataName> Env
	)
	{
		//- first K, F
		//-- for each pay elem:
		//--- GC (transitively?) old K, F -- any affected V, R already implicitly GC?
		//--- add new K, F

		Set<AssrtVar> Kself = K.get(self);
		Set<AssrtBFormula> Fself = F.get(self);
		for (PayElemType<?> e : ((EAction) a).payload.elems)  // CHECKME: EAction closest base type
		{
			if (e instanceof AssrtAnnotDataName)
			{
				AssrtAnnotDataName cast = (AssrtAnnotDataName) e;
				AssrtVar v = cast.var;
				gcF(Fself, v);
				Kself.add(v);
				Env.put(v, cast.data);
			}
			else
			{
				throw new RuntimeException("[assrt-core] Shouldn't get in here: " + a);  
						// Regular DataType pay elems have been given fresh annot vars (AssrtCoreGProtoDeclTranslator.parsePayload) -- no other pay elems allowed
			}
		}

		// Phantom payvars
		List<AssrtAnnotDataName> payPhant = a.getPhantoms();
		for (AssrtAnnotDataName p : payPhant)
		{
			// Duplicated from above for regular payvars
			AssrtVar v = p.var;
			gcF(Fself, v);
			Kself.add(v);
			Env.put(v, p.data);
		}

		// (Source of) `ass` is the difference between output and input -- CHECKME: payvar vs. msg? or old?
		Fself.add(ass);  // N.B. must come after adding phantoms (specifically, gcF; cf. AssrtCoreTest2, Test034)
		Fself.add(a.getPhantomAssertion());
		compactF(Fself);  // GC's true -- CHECKME: old "_" vars still relevant?

		//- then V, R
		//- for each state var
		//--- if continue
		//---- GC (transitively?) old V, R (also K, F?) -- outer statevars implicitly won't be affected
		//--- add V, R

		// "forward" recs will have state vars (svars) but no action state-exprs (aexprs)
		AssrtEState s = (AssrtEState) succ.curr;
		Map<AssrtVar, AssrtAFormula> Vself = V.get(self);
		Set<AssrtBFormula> Rself = R.get(self);  // Cf. Fself

		LinkedHashMap<AssrtVar, AssrtAFormula> svars = s.getStateVars();
		List<AssrtAFormula> aexprs = a.getStateExprs();
				// Following must come after F update (addAnnotBexprToF)
				// Update R from state -- entering a rec "forwards", i.e., not via a continue
		if (!svars.isEmpty() || !aexprs.isEmpty())
		{
			//Map<AssrtIntVar, AssrtAFormula> Vself_orig = new HashMap<>(Vself);
			//boolean isEntry = aexprs.isEmpty() && !svars.isEmpty();  
			boolean isContinue = !aexprs.isEmpty();
					// Rec-entry: expr args already inlined into the rec statevars (i.e., by proto inlining) -- CHECKME: means "forwards entry?" robust?  refactor?
			// CHECKME: now always true?, cf. AssrtCoreEGraphBuiler.buildEdgeAndContinuation AssrtCoreLRec `cont` f/w-rec case

			Iterator<AssrtAFormula> i = aexprs.iterator();
			for (Entry<AssrtVar, AssrtAFormula> e : svars.entrySet())
			{
				AssrtVar svar = e.getKey();
				AssrtAFormula sexpr = null;
				if (aexprs.isEmpty())  // "Init" state var expr -- must be a "constant"
				{
					//sexpr = e.getValue();
					sexpr = AssrtSGraphBuilderUtil.renameIntVarAsFormula(svar);
				}
				else
				{
					AssrtAFormula next = i.next();
					if (next.isConstant())
					// Deprecated special case treatment of statevar init exprs and "constant propagation" from model building -- now restored
					// Original intuition was to model "base case" and "induction step", but this is incompatible with unsat checking + loop counting
					{
						//sexpr = AssrtCoreSGraphBuilderUtil.renameIntVarAsFormula(svar);
						sexpr = next;
					}
					else
					{
						sexpr = (AssrtAFormula) AssrtSGraphBuilderUtil.renameFormula(next);
					}
				}
				if (isContinue)  // CHECKME: "shadowing", e.g., forwards statevar has same name as a previous
				{
					// CHECKME: inside loop?
					gcVR(Vself, Rself, svar);  // GC V , sexpr may be different than that removed
					gcF(Fself, svar);
				}
				Vself.put(svar, sexpr);
			}
		}

		// State phantoms
		LinkedHashMap<AssrtVar, AssrtAFormula> phantom = s.getPhantoms();  // TODO: hardcoded to int (no syntax for sorts)
		for (Entry<AssrtVar, AssrtAFormula> e : phantom.entrySet())
		{
			AssrtVar v = e.getKey();
			AssrtAFormula sexpr = AssrtSGraphBuilderUtil
					.renameIntVarAsFormula(v);  // Initialiser discarded
			Vself.put(v, sexpr);
			// CHECKME: compact? gc? cf. above
		}
		
		//if (!svars.isEmpty() || !aexprs.isEmpty() || !phantom.isEmpty())  // CHECKME
		{
			AssrtBFormula tmp = s.getAssertion();
			Rself.add(tmp);
			//compactR(Rself);  // TODO? (see above)
			compactF(Rself);
		}
	}
	
	private void gcF(Set<AssrtBFormula> Fself, AssrtVar v) 
	{
		Iterator<AssrtBFormula> i = Fself.iterator();
		while (i.hasNext())
		{
			if (i.next().getIntVars().contains(v)) 
			{
				i.remove();  // CHECKME: do transitively for vars in removed formula?
			}
		}
	}
	
	private void gcVR(Map<AssrtVar, AssrtAFormula> Vself, Set<AssrtBFormula> Rself, AssrtVar v)
	{
		Vself.remove(v);
		Iterator<AssrtBFormula> i = Rself.iterator();
		while (i.hasNext())
		{
			if (i.next().getIntVars().contains(v)) 
			{
				i.remove();  // CHECKME: do transitively for vars in removed formula?
			}
		}
	}

	private static void compactF(Set<AssrtBFormula> Fself)
	{
		Iterator<AssrtBFormula> i = Fself.iterator();
		while (i.hasNext())
		{
			AssrtBFormula f = i.next();
			if (f.equals(AssrtTrueFormula.TRUE) 
					|| f.getIntVars().stream().anyMatch(x -> x.toString().startsWith("_"))  // CHECKME: still needed?
					) 
			// CHECKME: other sources of renaming? makeFreshIntVar, and AssrtCoreSGraphBuilderUtil::renameFormula
			{
				i.remove();
			}
		}
	}

	
	/* Error checks */

	// Need to consider hasPendingRequest? -- no: the semantics blocks both sides until connected, so don't need to validate those "intermediate" states
	//public boolean isReceptionError()
	@Override
	public Map<Role, ? extends AssrtERecv> getStuckMessages()
	{
		Map<Role, AssrtERecv> res = new HashMap<>();
		for (Role self : this.efsms.keySet())
		{
			EFsm s = this.efsms.get(self);
			EStateKind k = s.curr.getStateKind();
			if (k == EStateKind.UNARY_RECEIVE || k == EStateKind.POLY_RECIEVE)
			{
				Role peer = s.curr.getActions().get(0).peer;  // Pre: consistent ext choice subj
				AssrtESend msg = (AssrtESend) this.queues.getQueue(self).get(peer);
				if (msg != null)
				{
					AssrtERecv dual = msg//.toTrueAssertion()
							.toDual(peer);  // N.B. toTrueAssertion
					//if (!s.curr.hasAction(dual))  // CHECKME: ...map(a -> ((AssrtCoreESend) a.toDual(dst)).toTrueAssertion()) ?
					// TODO: check assertion implication (not just syntactic equals) -- cf. AssrtSConfig::fire
					if (s.curr.getActions().stream()
							.noneMatch(x -> ((AssrtERecv) x).dropStateArgs().equals(dual)))
					{
						res.put(self, msg.toDual(peer));  // "Original" message
					}
				}
			}
		}
		return res;
	}
	
	// Cf. refactor syntactically?  cf. connection errors
	public Map<Role, Set<AssrtEAction>> getUnknownDataVarErrors(
			AssrtCore core, GProtoName fullname)
	{
		Map<Role, Set<AssrtEAction>> res = new HashMap<>();
		for (Entry<Role, EFsm> e : this.P.entrySet())
		{
			Role self = e.getKey();
			EState curr = e.getValue().curr;
			Set<AssrtVar> Kself = this.K.get(self);
			Set<AssrtVar> Vself = this.V.get(self).keySet();
			/*Set<String> rs = core.getContext().getInlined(fullname).roles.stream()
					.map(Object::toString).collect(Collectors.toSet());*/
			Predicate<EAction> isErr = a ->
			{
				if (a.isSend() || a.isRequest())
				{
					Set<AssrtVar> known = a.payload.elems.stream()
							.map(x -> ((AssrtAnnotDataName) x).var)
							.collect(Collectors.toSet());
						// TODO: throw new RuntimeException("[assrt-core] Shouldn't get in here: " + pe);
					known.addAll(Kself);
					known.addAll(Vself);
					return ((AssrtEAction) a).getAssertion().getIntVars().stream()
							//.filter(x -> !rs.contains(x.toString()))  // CHECKME: formula role vars -- what is this for?  what is an example?
							.anyMatch(x -> !known.contains(x));
				}
				else
				{
					return false;  // CHECKME: input-side assertions? currently hardcoded to True
				}
			};

			Set<AssrtEAction> tmp = curr.getDetActions().stream()
					.filter(x -> isErr.test(x)).map(x -> (AssrtEAction) x)
					.collect(Collectors.toSet());
			if (!tmp.isEmpty())
			{
				res.put(self, tmp);
			}
		}
		return res;
	}
	
	// N.B. actually a safety error, not a "progress" error
	// i.e., output state has a "well-asserted" action
	public Map<Role, EState> getAssertProgressErrors(AssrtCore core,
			GProtoName fullname)
			// CHECKME: not actually a "progress" error -- "safety"?
	{
		AssrtGProtocol proto = ((AssrtGProtocol) core.getContext().getInlined(fullname));
		// Could try to update Env with statevars from node labels, cf. AssrtEState#getStateVars
		Map<AssrtVar, DataName> sorts = proto.getSortEnv();  // Must do on proto for outermost statevars (inlined does not embed top-level statevars as a rec)
		//return this.P.entrySet().stream().anyMatch(e ->  // anyMatch is on the endpoints (not actions)
		Map<Role, EState> res = new HashMap<>();
		for (Entry<Role, EFsm> e : this.P.entrySet())
		{
			Role self = e.getKey();
			EState curr = e.getValue().curr;
			AssrtBFormula squashed = getAssertProgressCheck(core, fullname, self,
					curr);
			if (squashed.equals(AssrtTrueFormula.TRUE))
			{
				continue;
			}

			core.verbosePrintln("\n[assrt-core] Checking assertion progress for "
					+ self + "(" + curr.id + "):");
			core.verbosePrintln("  squashed = " + squashed.toSmt2Formula(sorts));
			if (!core.checkSat(fullname,
					Stream.of(squashed).collect(Collectors.toSet())))
			{
				res.put(self, curr);
			}
			core.verbosePrintln("");
		}
		return res;
	}

	// formula: isAssertionProgressSatisfied (i.e., true = OK)
	private AssrtBFormula getAssertProgressCheck(AssrtCore core,
			GProtoName fullname, Role self, EState curr)
	{
		//if (as.isEmpty() || as.stream().noneMatch(a -> a.isSend() || a.isRequest())) 
		if (curr.isTerminal() || curr.getStateKind() != EStateKind.OUTPUT)  // CHECKME: only output states?
		{
			return AssrtTrueFormula.TRUE;
		}
		List<EAction> as = curr.getActions();  // N.B. getActions includes non-fireable
		if (as.stream().anyMatch(x -> x instanceof EDisconnect))
		{
			throw new RuntimeException("[assrt-core] [TODO] Disconnect actions: " + as);
		}

		// lhs = conjunction of F terms, V eq-terms and R terms -- i.e., what we already know
		Set<AssrtBFormula> Fself = this.F.get(self);
		AssrtBFormula lhs = Fself.isEmpty()
				? AssrtTrueFormula.TRUE
				: Fself.stream().reduce((x1, x2) -> AssrtFormulaFactory
						.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2)).get();  // Here, conjunction of F terms
		Map<AssrtVar, AssrtAFormula> Vself = this.V.get(self);
		if (!Vself.isEmpty())
		{
			AssrtBFormula vconj = Vself.entrySet().stream()
					.map(x -> (AssrtBFormula) AssrtFormulaFactory.AssrtBinComp(  // (Cast needed for reduce)
							AssrtBinCompFormula.Op.Eq,
							AssrtFormulaFactory.AssrtIntVar(x.getKey().toString()),
							x.getValue()))
					.reduce((e1, e2) -> (AssrtBFormula) AssrtFormulaFactory  // Next, conjunction of V eq-terms
							.AssrtBinBool(AssrtBinBFormula.Op.And, e1, e2))
					.get();  // Non-empty
			lhs = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs, vconj);
		}
		Set<AssrtBFormula> Rself = this.R.get(self);
		if (!Rself.isEmpty())
		{
			AssrtBFormula Rconj = Rself.stream().reduce((b1, b2) -> AssrtFormulaFactory  // Next, conjunction of R terms
					.AssrtBinBool(AssrtBinBFormula.Op.And, b1, b2)).get();  // Non-empty
			lhs = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs, Rconj);
		}
		
		// CHECKME: why V/R not also built here? cf. getRecAssertCheck-- lhs building should be uniform for all checks?

		// rhs = disjunction of assertions (ex-qualified by pay annot vars) from each action -- i.e., what we would like to do
		AssrtBFormula rhs = null;
		for (EAction a : as)
		{
			if (!(a instanceof AssrtESend)) // TODO: && !(a instanceof AssrtCoreEReq))
			{
				throw new RuntimeException("[assrt-core] Shouldn't get in here: " + a);
			}
			AssrtBFormula ass = ((AssrtEAction) a).getAssertion();
			if (ass.equals(AssrtTrueFormula.TRUE))
			{
				return AssrtTrueFormula.TRUE;  // If any assertion is True, then assertion-progress trivially satisfied
			}

			/*Set<AssrtIntVarFormula> assVars = a.payload.elems.stream()
					.map(x -> AssrtFormulaFactory
							.AssrtIntVar(((AssrtAnnotDataName) x).var.toString()))
					.collect(Collectors.toSet());  // ex-qualify pay annot vars, this will be *some* set of values
					// N.B. includes the case for recursion cycles where var is "already" in F
					// CHECKME: Adding even if var not used?*/
			Set<AssrtAVarFormula> assVars = getAssVars(a);

			if (!assVars.isEmpty()) // CHECKME: currently never empty
			{
				ass = AssrtFormulaFactory.AssrtExistsFormula(new LinkedList<>(assVars),
						ass);
			}
			rhs = (rhs == null)
					? ass
					: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.Or, rhs, ass);
		}

		AssrtBFormula impli = AssrtFormulaFactory
				.AssrtBinBool(AssrtBinBFormula.Op.Imply, lhs, rhs);
		return forallQuantifyFreeVars(core, fullname, impli)
				.squash();  // Finally, fa-quantify all free vars
	}

	private Set<AssrtAVarFormula> getAssVars(EAction a)
	{
		Set<AssrtAVarFormula> assVars = new HashSet<>();
		for (PayElemType<? extends PayElemKind> e : a.payload.elems)
		{
			AssrtAnnotDataName d = (AssrtAnnotDataName) e;
			switch (d.data.toString()) // TODO: refactor
			{
			case "int":
			case "String":
			case "string":
				assVars.add(AssrtFormulaFactory.AssrtIntVar(d.var.toString()));
				break;
			/*assVars.add(AssrtFormulaFactory.AssrtStrVar(d.var.toString()));
			break;*/
			default:
				throw new RuntimeException(
						"[assrt-core] Unsupported data type: " + d.data);
			}
		}
		return assVars;
	}
	
	// For batching?
	protected Set<AssrtBFormula> getAssertProgressChecks(AssrtCore core,
			GProtoName fullname)
	{
		return this.P.entrySet().stream().map(e ->  // anyMatch is on the endpoints (not actions)
		getAssertProgressCheck(core, fullname, e.getKey(), e.getValue().curr)
		).collect(Collectors.toSet());
	}

	// i.e., state has an action that is not satisfiable (deadcode)
	public Map<Role, Set<AssrtEAction>> getAssertUnsatErrors(AssrtCore core,
			GProtoName fullname)
	{
		AssrtGProtocol proto = ((AssrtGProtocol) core.getContext()
				.getInlined(fullname));
		// Could try to update Env with statevars from node labels, cf. AssrtEState#getStateVars
		Map<AssrtVar, DataName> sorts = proto.getSortEnv();  // Must do on proto for outermost statevars (inlined does not embed top-level statevars as a rec)
		Map<Role, Set<AssrtEAction>> res = new HashMap<>();
		for (Entry<Role, EFsm> e : this.P.entrySet())
		{
			Role self = e.getKey();
			EState curr = e.getValue().curr;
			if (curr.getStateKind() != EStateKind.OUTPUT)
			// Only check unsat on the output side, where the choice is being made, input side should "follow"
			// Cannot actually check on input side, e.g., recursive choice of b>=0 || b<0, after one loop one action can be unsat w.r.t. F-knowledge of previously chosen action
			// May need at least three parties (or some other asynchrony) to expose (need an "intermediate" state where input role is "passively waiting" with prev-K and new-actions)
			// E.g., SH -- cf. good.extensions.assrtcore.safety.unsat.AssrtCoreTest35 
			// TODO: factor out with getAssertSatChecks
			// N.B. this is the only "get errors" operation to impose "additional conditions" on top of the "get check", cf. assert-prog, rec-assert don't -- TODO refactor into "get check"?
			{
				continue;
			}
			List<EAction> as = curr.getActions(); // N.B. getActions includes non-fireable
			if (as.size() <= 1)  // Optimisation // TODO: factor out with getAssertSatChecks
			// Can do only on non-unary choices -- for unary, assrt-prog implies assrt-sat
					// Note: this means "downstream" assrt-unsat errors for unary-choice continuations will not be caught (i.e., false => false for assrt-prog)
			{
				continue;  // CHECKME -- No: for state-vars and state-assertions? Is it even definitely skippable without those?
			}
			if (as.stream().anyMatch(x -> x instanceof EDisconnect))
			{
				throw new RuntimeException(
						"[assrt-core] [TODO] Disconnect actions: " + as);
			}
			
			for (EAction a : as)
			{
				AssrtEAction cast = (AssrtEAction) a;
				AssrtBFormula squashed = getAssertSatCheck(core, fullname, self, cast);
				if (squashed.equals(AssrtTrueFormula.TRUE))  // OK to skip? i.e., no need to check existing F (impli LHS) is true?
				{
					continue; 
				}

				core.verbosePrintln(
						"\n[assrt-core] Checking assertion satisfiability for " + self
								+ "(" + curr.id + "):");
				core.verbosePrintln("  squashed = " + squashed.toSmt2Formula(sorts));
				if (!core.checkSat(fullname,
						Stream.of(squashed).collect(Collectors.toSet())))
				{
					Set<AssrtEAction> tmp = res.get(self);
					if (tmp == null)
					{
						tmp = new HashSet<>();
						res.put(self, tmp);
					}
					tmp.add(cast);
				}
				core.verbosePrintln("");
			}
		}
		return res;
	}

	// formula: isSatisfiable (i.e., true = OK)
	private AssrtBFormula getAssertSatCheck(AssrtCore core, GProtoName fullname,
			Role self, AssrtEAction a)
	{
		AssrtBFormula ass = a.getAssertion();
		if (ass.equals(AssrtTrueFormula.TRUE))  // OK to skip? i.e., no need to check existing F (impli LHS) is true?
		{
			return AssrtTrueFormula.TRUE; 
		}

		// Here, action assertion, ex-quantified by pay annot vars
		AssrtBFormula res = ass;
		Set<AssrtVarFormula> varsA = ((EAction) a).payload.elems.stream()
				.map(x -> AssrtFormulaFactory
						.AssrtIntVar(((AssrtAnnotDataName) x).var.toString()))
				.collect(Collectors.toSet());
				// N.B. includes the case for recursion cycles where var is "already" in F
				// Adding even if var not used
		if (!varsA.isEmpty()) // CHECKME: currently never empty
		{
			res = AssrtFormulaFactory.AssrtExistsFormula(new LinkedList<>(varsA),
					res);
		}

		// Next, conjunction of F terms -- CHECKME: always non-empty?
		res = this.F.get(self).stream().reduce(res, (b1, b2) -> AssrtFormulaFactory
				.AssrtBinBool(AssrtBinBFormula.Op.And, b1, b2));

		// Next, conjunction of V eq-terms
		// Include Vself and Rself, to check lhs(?) is sat for assrt-prog (o/w false => any)
		Map<AssrtVar, AssrtAFormula> Vself = this.V.get(self);
		if (!Vself.isEmpty())
		{  // Cast needed for reduce
			AssrtBFormula Vconj = Vself.entrySet().stream()
					.map(x -> (AssrtBFormula) AssrtFormulaFactory.AssrtBinComp(
							AssrtBinCompFormula.Op.Eq,
							AssrtFormulaFactory.AssrtIntVar(x.getKey().toString()),
							x.getValue()))
					.reduce((e1, e2) -> (AssrtBFormula) AssrtFormulaFactory
							.AssrtBinBool(AssrtBinBFormula.Op.And, e1, e2))
					.get();  // non-empty
			res = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And,
					res, Vconj);
		}

		// Next, conjunction of R terms
		Set<AssrtBFormula> Rself = this.R.get(self);
		if (!Rself.isEmpty())
		{
			AssrtBFormula Rconj = Rself.stream()
					.reduce((b1, b2) -> AssrtFormulaFactory
							.AssrtBinBool(AssrtBinBFormula.Op.And, b1, b2))
					.get();  // Non-empty
			res = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, res,
					Rconj);
		}

		// Finally, *ex*-quanitfy all free vars -- cf. forallQuantifyFreeVars
		/*Set<String> rs = core.getContext().getInlined(fullname).roles.stream()
				.map(Object::toString).collect(Collectors.toSet());*/
		Set<AssrtVar> free = res.getIntVars().stream()
				//.filter(x -> !rs.contains(x.toString()))  // CHECKME: formula role vars -- cf. getUnknownDataVarErrors  // CHECKME: what is the example?
				.collect(Collectors.toSet());
		if (!free.isEmpty())
		{
			res = AssrtFormulaFactory.AssrtExistsFormula(
					free.stream().map(v -> AssrtFormulaFactory.AssrtIntVar(v.toString()))
							.collect(Collectors.toList()),
					res);  // Cf. assrt-prog -- here, don't need action to be sat *forall* prev, just sat for *some* prev
		}

		return res.squash();
	}
	
	// For batching
	public Set<AssrtBFormula> getAssertSatChecks(AssrtCore core,
			GProtoName fullname)
	{
		return this.P.entrySet().stream()
				// Consistent with getAssertUnsatErrors -- TODO refactor
				.filter(x -> x.getValue().curr.getStateKind() == EStateKind.OUTPUT  // Cannot check on input side, see getAssertUnsatErrors
						&& x.getValue().curr.getActions().size() > 1)  // Optimisation
				.flatMap(e ->  // anyMatch is on the endpoints (not actions)
				e.getValue().curr.getActions().stream().map(a ->
				getAssertSatCheck(core, fullname, e.getKey(), (AssrtEAction) a))
		).collect(Collectors.toSet());
	}

  // Otherwise initial assertions not checked, since no incoming action (cf. below)
	public Map<Role, AssrtEState> getInitRecAssertErrors(AssrtCore core,
			GProtoName fullname)
	{
		AssrtGProtocol proto = ((AssrtGProtocol) core.getContext()
				.getInlined(fullname));
		// Could try to update Env with statevars from node labels, cf. AssrtEState#getStateVars
		Map<AssrtVar, DataName> sorts = proto.getSortEnv();  // Must do on proto for outermost statevars (inlined does not embed top-level statevars as a rec)
		Map<Role, AssrtEState> res = new HashMap<>();
		for (Entry<Role, EFsm> e : this.P.entrySet())
		{
			Role self = e.getKey();
			AssrtEState curr = (AssrtEState) e.getValue().curr;
			AssrtBFormula toCheck = getInitRecAssertCheck(core, fullname, self, curr);
			if (toCheck.equals(AssrtTrueFormula.TRUE))
			{
				continue;
			}

			core.verbosePrintln(
					"\n[assrt-core] Checking initial recursion assertion for " + self
							+ "(" + curr.id + "):");
			core.verbosePrintln("  squashed = " + toCheck.toSmt2Formula(sorts));
			if (!core.checkSat(fullname,
					Stream.of(toCheck).collect(Collectors.toSet())))
			{
				res.put(self, curr);
			}
			core.verbosePrintln("");
		}
		return res;
	}

	// formula: isNotRecursionAssertionSatisfied (i.e., true = OK)
	protected AssrtBFormula getInitRecAssertCheck(AssrtCore core,
			GProtoName fullname, Role self, AssrtEState curr)
	{
		AssrtBFormula toCheck = curr.getAssertion().squash();
		if (toCheck.equals(AssrtTrueFormula.TRUE))
		{
			return AssrtTrueFormula.TRUE;
		}

		// CHECKME: using proj (local type, not only CFSM)
		AssrtLProjection proj = (AssrtLProjection) core.getContext()
				.getProjectedInlined(fullname, self);
		// CHECKME: some redundancy between nested top-level LRec and LProjection info -- but maybe conflict with rec pruning
		AssrtLType body = proj.type;  // Guaranteed AssrtCoreLRec?
		LinkedHashMap<AssrtVar, AssrtAFormula> svars = proj.statevars;
		// Also need to collect svars from (immediately) nested recs -- i.e., svars from a subproto that actually becomes the top-level init state due to projection
		LinkedHashMap<AssrtVar, AssrtAFormula> phantom = proj.phantom;
		while (body instanceof AssrtLRec)  // CHECKME: loop necessary?
		{
			AssrtLRec cast = (AssrtLRec) body;
			svars.putAll(cast.statevars);
			phantom.putAll(cast.phantom);
			body = cast.body;
		}

		Map<AssrtVar, AssrtAFormula> Vself = this.V.get(self);
		if (!Vself.isEmpty())
		{
			Function<AssrtVar, AssrtAFormula> getInit = x -> svars.containsKey(x)
					? svars.get(x) : phantom.get(x);  // CHECKME: use initialiser for phantom, should be exist quant instead?
			AssrtBFormula Vconj = Vself.entrySet().stream().map(x -> (AssrtBFormula)  // Cast needed
					AssrtFormulaFactory.AssrtBinComp(AssrtBinCompFormula.Op.Eq,
					AssrtFormulaFactory.AssrtIntVar(x.getKey().toString()), //x.getValue()
					//svars.get(x.getKey())
					getInit.apply(x.getKey())))
					// Special case treatment of statevar init exprs and "constants" deprecated from model building
					// So have to look up init exprs "manually"
					// Original intuition was to model "base case" and "induction step", but this is incompatible with unsat checking + (e.g.) loop counting
					.reduce((b1, b2) -> AssrtFormulaFactory
							.AssrtBinBool(AssrtBinBFormula.Op.And, b1, b2))
							// do-statevar expr args for "forwards" rec already inlined into rec-statevars
					.get();
			// Currently allowing rec-assertion without any statevardecls (i.e., cannot use any vars), but pointless?
			toCheck = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.Imply,
					Vconj, toCheck);
		}
		return forallQuantifyFreeVars(core, fullname, toCheck).squash();
	}			

	// For batching
	public Set<AssrtBFormula> getInitRecAssertChecks(AssrtCore core,
			GProtoName fullname)
	{
		//if (isInit)
		{
			return this.P.entrySet().stream().map(e ->  // anyMatch is on the endpoints (not actions)
			getInitRecAssertCheck(core, fullname, e.getKey(),
					(AssrtEState) e.getValue().curr)
			).collect(Collectors.toSet());
		}
	}

	public Set<AssrtBFormula> getRecAssertChecks(AssrtCore core,
			GProtoName fullname)
	{
		return this.P.entrySet().stream().flatMap(e ->  // anyMatch is on the endpoints (not actions)
		e.getValue().curr.getActions().stream()
				.map(a -> getRecAssertCheck(core, fullname, e.getKey(),
						(AssrtEState) e.getValue().curr, (AssrtEAction) a)))
				.collect(Collectors.toSet());
	}

	// Pre: stuckMessages checked
	// CHECKME: equivalent of assertion progress for rec-assertions?  unsat not needed for recs? (because for "unary-choice" they coincide?)
	// Excluding "init" rec
	public Map<Role, Set<AssrtEAction>> getRecAssertErrors(AssrtCore core,
			GProtoName fullname)
	{
		AssrtGProtocol proto = ((AssrtGProtocol) core.getContext()
				.getInlined(fullname));
		// Could try to update Env with statevars from node labels, cf. AssrtEState#getStateVars
		Map<AssrtVar, DataName> sorts = proto.getSortEnv();  // Must do on proto for outermost statevars (inlined does not embed top-level statevars as a rec)
		Map<Role, Set<AssrtEAction>> res = new HashMap<>();
		for (Entry<Role, EFsm> e : this.P.entrySet())
		{
			Role self = e.getKey();
			AssrtEState curr = (AssrtEState) e.getValue().curr;
			Predicate<EAction> isSat = a ->  // false = error
			{
				AssrtEAction cast = (AssrtEAction) a;
				AssrtBFormula toCheck = getRecAssertCheck(core,
						fullname, self, curr, cast);
				if (toCheck.equals(AssrtTrueFormula.TRUE))
				{
					return true;
				}
					
				core.verbosePrintln("\n[assrt-core] Checking recursion assertion for "
						+ self + "(" + curr.id + "):");
					core.verbosePrintln("  squashed = " + toCheck.toSmt2Formula(sorts));
				boolean b = core.checkSat(fullname,
						Stream.of(toCheck).collect(Collectors.toSet()));
				core.verbosePrintln("");
				return b;
			};
			EStateKind kind = curr.getStateKind();
			if (kind == EStateKind.OUTPUT || 
					kind == EStateKind.UNARY_RECEIVE
					|| kind == EStateKind.POLY_RECIEVE
					)
			{
				Set<AssrtEAction> tmp = curr.getActions().stream()
						.filter(x -> !isSat.test(x)).map(x -> (AssrtEAction) x)
						.collect(Collectors.toSet());
				if (!tmp.isEmpty())
				{
					res.put(self, tmp);
				}
			}
			else if (kind != EStateKind.TERMINAL)
			{
				throw new RuntimeException(
						"\n[assrt-core] Shouldn't get in here: " + kind);
			}
		}
		return res;
	}

	// N.B. curr is the state before the rec-entry: curr -a-> rec-entry
	// Pre: 'curr/a' is output kind, or 'curr' is input kind and 'a' is toDual of enqueued message
	// formula: isNotRecursionAssertionSatisfied (i.e., true = OK)
	protected AssrtBFormula getRecAssertCheck(AssrtCore core, GProtoName fullname,
			Role self, AssrtEState curr, AssrtEAction a)
	{
		//EStateKind kind = curr.getStateKind();
		EAction cast = (EAction) a;
		if (cast.isReceive() || cast.isAccept())
		{
			/*// Now redundant, checked by getRecAssertErrors -- no?
			// CHECKME: "skip" if no msg avail (because assertion carried by msg? -- no)
			// ^what is an example? -- due to async, e.g., input-side rec-assert checked twice w.r.t. state that does peer output (no message yet) and actual input state (message arrived)*/
			if (!this.Q.isConnected(self, ((EAction) a).peer)
					|| this.Q.getQueue(self).get(cast.peer) == null)
				 // !isPendingRequest(a.peer, self))  // TODO: open/port annots
			{
				return AssrtTrueFormula.TRUE;
			}
		}
		else if (!(cast.isSend() || cast.isRequest()))
		{
			throw new RuntimeException(
					"[assrt-core] [TODO] " + cast.getClass() + ":\n\t" + cast);
		}
		AssrtEState succ = curr.getDetSucc(cast);
		AssrtBFormula sass = succ.getAssertion().squash();
		if (sass.equals(AssrtTrueFormula.TRUE))
		{
			return AssrtTrueFormula.TRUE;
		}

		// lhs = ...  // TODO: factor out lhs building with others above -- CHECKME lhs should be same for all?
		// Here, conjunction of F terms
		AssrtBFormula lhs = null;
		Set<AssrtBFormula> Fself = this.F.get(self);
		if (!Fself.isEmpty())
		{
			lhs = Fself.stream().reduce((x1, x2) -> AssrtFormulaFactory
					.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2)).get();
		}
		
		// Next, conjunction of V eq-terms
		Map<AssrtVar, AssrtAFormula> Vself = this.V.get(self);
		if (!Vself.isEmpty())
		{
			AssrtBFormula Vconj = Vself.entrySet().stream()
					.map(x -> (AssrtBFormula) AssrtFormulaFactory.AssrtBinComp(  // Cast needed for reduce
							AssrtBinCompFormula.Op.Eq,
							AssrtFormulaFactory.AssrtIntVar(x.getKey().toString()),
							x.getValue()))
					.reduce(
							(x1, x2) -> (AssrtBFormula) AssrtFormulaFactory
									.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2)
				).get();
			lhs = (lhs == null) 
					? Vconj
					: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs,
							Vconj);
		}
		
		// Next, assertion from action (carried by msg for input actions)
		AssrtBFormula aass = /*(cast.isSend() || cast.isRequest())  // CHECKME: AssrtEAction doesn't have those methods, refactor?
				? a.getAssertion()
				: //(a.isReceive() || a.isAccept())  // Has msg/req already checked at top
					((AssrtCoreEMsg) this.Q.getQueue(self).get(cast.peer)).getAssertion();
					// Cf. updateInput, msg.getAssertion()*/
				a.getAssertion();  // Input-kind action already derived (toDual) from enqueued message by getRecAssertErrors
		if (!aass.equals(AssrtTrueFormula.TRUE))
		{
			lhs = (lhs == null) 
					? aass
					: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs, aass);
		}

		Set<AssrtBFormula> Rself = this.R.get(self);
		if (!Rself.isEmpty())
		{
			AssrtBFormula rtmp = Rself.stream().reduce(
					(x1, x2) -> AssrtFormulaFactory
							.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2))
					.get();
			lhs = (lhs == null)
					? rtmp
					: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs,
							rtmp);
		}
			
		List<AssrtAFormula> aexprs = a.getStateExprs();
		if (aexprs.isEmpty())  // "Forwards" rec entry -- cf. updateForAssertionAndStateExprs, updateRecEntry/Continue
		// TODO: now obsolete, cf. AssrtCoreEGraphBuiler.buildEdgeAndContinuation AssrtCoreLRec `cont` f/w-rec case
		{
			// Next for lhs, state var eq-terms for initial rec entry
			LinkedHashMap<AssrtVar, AssrtAFormula> svars = succ.getStateVars();
			if (!svars.isEmpty())
			{
				AssrtBFormula svarsConj = svars.entrySet().stream()
						.map(x -> (AssrtBFormula) AssrtFormulaFactory.AssrtBinComp(  // Cast needed
								AssrtBinCompFormula.Op.Eq,
								AssrtFormulaFactory.AssrtIntVar(x.getKey().toString()),
								x.getValue()))  // do-statevar expr args for "forwards" rec already inlined into rec-statevars
						.reduce((x1, x2) -> AssrtFormulaFactory
								.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2))
						.get();
				lhs = (lhs == null) 
						? svarsConj
						: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs,
								svarsConj);
			}

			// rhs = target state (succ) assertion
			AssrtBFormula rhs = sass;

			// Phantoms -- (non-phantom) statevars exist quantified above, now also exist quant phantoms (morally do not need to show phantoms constructively, but may need assertion)
			// TODO: factor out with below
			List<AssrtAVarFormula> phants = succ.getPhantoms().keySet().stream()
					.map(x -> AssrtFormulaFactory.AssrtIntVar(x.toString()))  // convert from AssrtIntVar to AssrtIntVarFormula -- N.B. AssrtIntVar now means var of any sort
					.collect(Collectors.toList());
			rhs = AssrtFormulaFactory.AssrtExistsFormula(phants, rhs);
			
			// CHECKME: factor out with below
			AssrtBFormula impli = (lhs == null) 
					? rhs
					: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.Imply, lhs,
							rhs);

			return forallQuantifyFreeVars(core, fullname, impli).squash();
		}
		else  // Rec-continue
		{
			// Next for lhs, rec ass (on original vars)  // e.g., lhs, ...x > 0... (for existing x) => exists ...x' > 0... (for new x')
			/*if (!sass.equals(AssrtTrueFormula.TRUE))
			{
				lhs = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, lhs,
						sass);
			}*/
			if (lhs == null)
			{
				lhs = AssrtTrueFormula.TRUE;
			}

			// rhs = existing R assertions -- CHECKME: why not just target rec ass again? (like entry)  or why entry does not check this.R and new entry ass -- i.e., why rec entry/continue not uniform?
					// ^FIXME: should not be existing or just entry -- should be all existing up to entry ?
			//Set<AssrtBFormula> Rself = this.R.get(self);   
					// Can use this.R because recursing, should already have all the terms to check (R added on f/w rec-entry updateRecEntry)
					// CHECKME: should it be *all* the terms so far? yes, because treating recursion assertions as invariants?
			//if (!Rself.isEmpty())
			/*AssrtBFormula rhs = Rself.stream().reduce(//AssrtTrueFormula.TRUE,  // "identity-reduce" variant convenient here for below test
					sass,
					(x1, x2) -> AssrtFormulaFactory
							.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2));
					// CHECKME: do check, even if AA is True?  To check state var update isn't a contradiction?
					// TODO: that won't be checked by this, lhs just becomes false -- this should be checked by unsat? (but that is only poly choices)*/
			AssrtBFormula rhs = sass;
			if (rhs.equals(AssrtTrueFormula.TRUE))
			{
				return AssrtTrueFormula.TRUE;  // CHECKME: move "shortcircuit" to top?
			}

			// Next for rhs, rename target rec state vars
			List<AssrtVar> old = new LinkedList<>(succ.getStateVars().keySet());  // TODO: state var ordering w.r.t. exprs
			List<AssrtVarFormula> fresh = old.stream()
					.map(AssrtSConfig::makeFreshIntVar)
					.collect(Collectors.toList());
			Iterator<AssrtVarFormula> ifresh = fresh.iterator();
			for (AssrtVar v : old)
			{
				rhs = rhs.subs(AssrtFormulaFactory.AssrtIntVar(v.toString()),
						ifresh.next());
			}
			
			// Next for rhs, conjunction of eq-terms between renamed succ state vars and action state exprs -- and renamed vars ex-quantified 
			Iterator<AssrtAFormula> iaexprs = aexprs.iterator();
			AssrtBFormula svarsConj = fresh.stream().map(x -> (AssrtBFormula)  // Cast needed
			AssrtFormulaFactory.AssrtBinComp(AssrtBinCompFormula.Op.Eq,
					AssrtFormulaFactory.AssrtIntVar(x.toString()), iaexprs.next()))
					.reduce((x1, x2) -> AssrtFormulaFactory
							.AssrtBinBool(AssrtBinBFormula.Op.And, x1, x2))
					.get();
			rhs = AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, rhs,
					svarsConj);
			rhs = AssrtFormulaFactory.AssrtExistsFormula(
					fresh.stream().map(x -> AssrtFormulaFactory.AssrtIntVar(x.toString()))  // HERE: factor out with getAssVars -- refactor to return AssrtAVar
							.collect(Collectors.toList()),
					rhs);

			// Phantoms -- (non-phantom) statevars exist quantified above, now also exist quant phantoms (morally do not need to show phantoms constructively, but may need assertion)
			// TODO: factor out with prev case
			List<AssrtAVarFormula> phants = succ.getPhantoms().keySet().stream()
					.map(x -> AssrtFormulaFactory.AssrtIntVar(x.toString()))  // convert from AssrtIntVar to AssrtIntVarFormula -- N.B. AssrtIntVar now means var of any sort
					.collect(Collectors.toList());
			rhs = AssrtFormulaFactory.AssrtExistsFormula(phants, rhs);

			AssrtBFormula impli = AssrtFormulaFactory
					.AssrtBinBool(AssrtBinBFormula.Op.Imply, lhs, rhs);
			return forallQuantifyFreeVars(core, fullname, impli).squash();
		}
	}
	
	
	/* toString, hashCode, equals */
	
	@Override
	public String toString()
	{
		return "(P=" + this.P + ",\nQ=" 
			+ this.Q.toString().replaceAll("\\\"", "\\\\\"")  // Because of @"..." syntax, need to escape the quotes
			+ ",\nK=" + this.K + ",\nF=" + this.F
			+ ",\nV=" + this.V
			+ ",\nR=" + this.R
			//+ ",\nrename=" + this.rename
			//+ ",\nscopes=" + this.scopes  // N.B. this.scopes not included in equals/hashCode
			+ ")";
	}
	
	@Override
	public final int hashCode()
	{
		int hash = 22279;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.K.hashCode();
		hash = 31 * hash + this.F.hashCode();
		hash = 31 * hash + this.V.hashCode();
		hash = 31 * hash + this.R.hashCode();
		// this.scopes not included
		return hash;
	}

	// Not using id, cf. ModelState -- CHECKME: use a factory pattern that associates unique states and ids? -- use id for hash, and make a separate "semantic equals"
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtSConfig))
		{
			return false;
		}
		AssrtSConfig them = (AssrtSConfig) o;
		return super.equals(o) && this.K.equals(them.K) && this.F.equals(them.F)
				&& this.V.equals(them.V) && this.R.equals(them.R);
	}

	@Override
	protected boolean canEquals(Object o)
	{
		return o instanceof AssrtSConfig;
	}
	
	
	// isActive(SState, Role) becomes isActive(EState)
	public static boolean isActive(EState s, int init)
	{
		return !isInactive(s, init);
	}
	
	private static boolean isInactive(EState s, int init)
	{
		return s.isTerminal()
				|| (s.id == init && s.getStateKind() == EStateKind.ACCEPT);
				// s.isTerminal means non-empty actions (i.e., edges) -- i.e., non-end (cf., fireable)
	}

	
	/* P/Q/K/F/V/R helpers */

	private static Map<Role, Set<AssrtVar>> copyK(
			Map<Role, Set<AssrtVar>> K)
	{
		return K.entrySet().stream().collect(
				Collectors.toMap(Entry::getKey, e -> new HashSet<>(e.getValue())));
	}

	private static Map<Role, Set<AssrtBFormula>> copyF(
			Map<Role, Set<AssrtBFormula>> F)
	{
		return F.entrySet().stream().collect(
				Collectors.toMap(Entry::getKey, e -> new HashSet<>(e.getValue())));
	}

	private static Map<Role, Map<AssrtVar, AssrtAFormula>> copyR(
			Map<Role, Map<AssrtVar, AssrtAFormula>> R)
	{
		return R.entrySet().stream().collect(
				Collectors.toMap(Entry::getKey, e -> new HashMap<>(e.getValue())));
	}

	private static Map<Role, Set<AssrtBFormula>> copyRass(
			Map<Role, Set<AssrtBFormula>> Rass)
	{
		return Rass.entrySet().stream().collect(
				Collectors.toMap(Entry::getKey, e -> new HashSet<>(e.getValue())));
	}

	/*private static Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> copyRename(
			Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename)
	{
		return rename.entrySet().stream().collect(
				Collectors.toMap(Entry::getKey, e -> new HashMap<>(e.getValue())));
	}

	private static Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> copyScopes(
			Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes)
	{
		return scopes.entrySet().stream().collect(Collectors.toMap(Entry::getKey,
				x -> new LinkedHashMap<>(x.getValue())));
	}*/

	
	/* Formula building helpers */

	private static AssrtVarFormula makeFreshIntVar(AssrtVar var)
	{
		return AssrtFormulaFactory
				.AssrtIntVar("_" + var.toString() + "__" + AssrtSConfig.counter++);  // HACK
	}

	// Only statevars?
	private static AssrtBFormula forallQuantifyFreeVars(AssrtCore core,
			GProtoName fullname, AssrtBFormula bform)
	{
		/*Set<String> rs = core.getContext().getInlined(fullname).roles.stream()
				.map(Object::toString).collect(Collectors.toSet());*/
		Set<AssrtVar> free = bform.getIntVars().stream()
				//.filter(x -> !rs.contains(x.toString()))  // CHECKME: formula role vars -- cf. getUnknownDataVarErrors  // CHECKME: what is an example?
				.collect(Collectors.toSet());
		if (!free.isEmpty())
		{
			List<AssrtAVarFormula> tmp = free.stream()
					.map(
							x -> 
							AssrtFormulaFactory.AssrtIntVar(x.toString())  // convert from AssrtIntVar to AssrtIntVarFormula -- N.B. AssrtIntVar now means var of any sort
					)
					.collect(Collectors.toList());
			bform = AssrtFormulaFactory.AssrtForallFormula(tmp, bform);
		}
		return bform;
	}

	/*
	  
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	 
	 */


	@Override
	public List<SConfig> sync(Role r1, EAction a1, Role r2, EAction a2)
	{
		throw new RuntimeException(
				"[assrt] TODO:\n\t" + r1 + " ,, " + a1 + "\n\t" + r2 + " ,, " + a2);
		/*List<SConfig> res = new LinkedList<>();
		List<EFsm> succs1 = this.efsms.get(r1).getSuccs(a1);
		List<EFsm> succs2 = this.efsms.get(r2).getSuccs(a2);
		if (succs1.size() > 1 || succs2.size() > 1)
		{
			throw new RuntimeException(
					"[assrt-core][TODO] Non-deteterministic actions not supported: "
							+ succs1 + " ,, " + succs2);
		}
		for (EFsm succ1 : succs1)
		{
			for (EFsm succ2 : succs2)
			{
				Map<Role, EFsm> efsms = new HashMap<>(this.efsms);
				// a1 and a2 are a "sync" pair, add all combinations of succ1 and succ2 that may arise
				efsms.put(r1, succ1);  // Overwrite existing r1/r2 entries
				efsms.put(r2, succ2);
				SingleBuffers queues;
				// a1 and a2 definitely "sync", now just determine whether it is a connect or wrap
				if (((a1.isRequest() && a2.isAccept())
						|| (a1.isAccept() && a2.isRequest())))
				{
					queues = this.queues.connect(r1, r2);  // N.B. queue updates are insensitive to non-det "a"
				}
				else if (((a1.isClientWrap() && a2.isServerWrap())
						|| (a1.isServerWrap() && a2.isClientWrap())))
				{
					// Doesn't affect queue state
					queues = this.queues;  // OK, immutable?
				}
				else
				{
					throw new RuntimeException("Shouldn't get in here: " + a1 + ", " + a2);
				}
				res.add(this.mf.global.SConfig(efsms, queues));
			}
		}
		return res;*/
	}

	/*// TODO: V
	private void fireAcc(Map<Role, EFsm> P, SSingleBuffers Q,
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,
			Map<Role, Set<AssrtVar>> K, Map<Role, Set<AssrtBFormula>> F, 
			Map<Role, Map<AssrtVarFormula, AssrtVarFormula>> rename,
			Role self, AssrtCoreEAcc a, EFsm succ)
	{
		throw new RuntimeException("[TODO] : " + a);
	//		P.put(self, succ);
	//		AssrtCoreEPendingRequest pr = (AssrtCoreEPendingRequest) Q.get(self)
	//				.put(a.peer, null);
	//		AssrtCoreEReq msg = pr.getMsg();  // CHECKME
	//		Q.get(a.peer).put(self, null);
	//
	//		updateInput(self, a, pr,  // msg?
	//				pr.shadow, succ, 
	//				K, F, V, R, rename);
	}*/

	/*private void fireReq(Map<Role, EFsm> P, SSingleBuffers Q,
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,
			Map<Role, Set<AssrtVar>> K, Map<Role, Set<AssrtBFormula>> F,
			Map<Role, Map<AssrtVarFormula, AssrtVarFormula>> rename,
			Role self, AssrtCoreEReq a, EFsm succ)
	{
		throw new RuntimeException("[TODO] : " + a);
	//		P.put(self, succ);
	//
	//		updateOutput(self, a, succ, K, F, V, R, rename);
	//
	//		Q.get(a.peer).put(self, new AssrtCoreEPendingRequest(a, rename.get(self)));  // Now doing toTrueAssertion on accept side
	}*/

	/*// Doesn't include pending requests, checks isInputQueueEstablished
	private boolean hasMsg(Role self, Role peer)
	{
		return isInputQueueEstablished(self, peer)  // input queue is established (not \bot and not <a>)
				&& this.Q.get(self).get(peer) != null;  // input queue is not empty
	}
	
	// Direction sensitive (not symmetric) -- isConnected means dest has established input queue from src
	// i.e. "fully" established, not "pending" -- semantics relies on all action firing being guarded on !hasPendingConnect
	private boolean isInputQueueEstablished(Role dest, Role src)  // N.B. is more like the "input buffer" at r1 for r2 -- not the actual "connection from r1 to r2"
	{
		AssrtCoreEMsg m = this.Q.get(dest).get(src);
		return !(m instanceof AssrtCoreEBot)
				&& !(m instanceof AssrtCoreEPendingRequest);
		//return es != null && es.equals(AssrtCoreEBot.ASSSRTCORE_BOT);  // Would be same as above
	}
	
	// req waiting for acc -- cf. reverse direction to isConnect
	private boolean isPendingRequest(Role req, Role acc)  // TODO: for open/port annotations
	{
		//return (this.ports.get(r1).get(r2) != null) || (this.ports.get(r2).get(r1) != null);
		AssrtCoreEMsg m = this.Q.get(acc).get(req);  // N.B. reverse direction to isConnected
		return m instanceof AssrtCoreEPendingRequest;
	}
	
	private boolean hasPendingRequest(Role req)
	{
		return this.Q.keySet().stream().anyMatch(acc -> isPendingRequest(req, acc));
	}*/

	/*// TODO: orphan pending requests -- maybe shouldn't?  handled by synchronisation error?
	//public boolean isOrphanError(Map<Role, AssrtEState> E0)
	public Map<Role, Set<? extends AssrtCoreESend>> getOrphanMessages()
	{
		Map<Role, Set<? extends AssrtCoreESend>> res = new HashMap<>();
		for (Role r : this.efsms.keySet())
		{
			Set<ESend> orphs = new HashSet<>();
			EFsm fsm = this.efsms.get(r);
			if (fsm.curr.isTerminal())  // Local termination of r, i.e. not necessarily "full deadlock cycle"
			{
				orphs.addAll(this.queues.getQueue(r).values().stream()
						.filter(v -> v != null).collect(Collectors.toSet()));
			}
			else
			{
				this.efsms.keySet().stream()
						.filter(x -> !r.equals(x) && !this.queues.isConnected(r, x))  // !isConnected(r, x), means r considers its side closed
						.map(x -> this.queues.getQueue(r).get(x)).filter(x -> x != null)  // r's side is closed, but remaining message(s) in r's buff
						.forEachOrdered(x -> orphs.add(x));
			}
			if (!orphs.isEmpty())
			{
				res.put(r, orphs);
			}
		}
		return res;
	}*/

	// Request/accept are bad if local queue is established
	// N.B. request/accept when remote queue is established is not an error -- relying on semantics to block until both remote/local queues not established; i.e., error precluded by design of model, not by validation
	// N.B. not considering pending requests, for the same reason as above and as why not considered for, e.g., reception errors -- i.e., not validating those "intermediate" states
	// Deadlock/progress errors that could be related to "decoupled" connection sync still caught by existing checks, e.g., orphan message or unfinished role
	@Deprecated
	public boolean isConnectionError()
	{
		throw new RuntimeException("Deprecated");
		/*return this.P.entrySet().stream().anyMatch(e -> 
				e.getValue().getActions().stream().anyMatch(a ->
						(a.isRequest() || a.isAccept()) && isInputQueueEstablished(e.getKey(), a.peer)
		));
				// TODO: check for pending port, if so then port is used -- need to extend an AnnotEConnect type with ScribAnnot (cf. AnnotPayloadType)*/
	}

	// Send is bad if either local queue or remote queue is not established, and no pending request to the target
	// Receive is bad if local queue is not established and no pending request to the target
	@Deprecated
	public boolean isUnconnectedError()
	{
		throw new RuntimeException("Deprecated");
		/*return this.P.entrySet().stream().anyMatch(e -> 
		{
			Role r = e.getKey();
			return 
					e.getValue().getActions().stream().anyMatch(a ->
									(a.isSend() &&
										(!isInputQueueEstablished(r, a.peer) || !isInputQueueEstablished(a.peer, r)) && !isPendingRequest(r, a.peer))
							|| (a.isReceive() && !isInputQueueEstablished(r, a.peer) && !isPendingRequest(r, a.peer)));
			// Don't need to use isPendingRequest(a.peer, r) because only requestor is "at the next state" while request pending 
		});*/
	}

	// "Connection message" reception error
	@Deprecated
	public boolean isSynchronisationError()
	{
		throw new RuntimeException("Deprecated");
		/*return this.P.entrySet().stream().anyMatch(e ->  // e: Entry<Role, EState>
		{
			EState s = e.getValue();
			EStateKind k = s.getStateKind();
			if (k != EStateKind.ACCEPT)
			{
				return false;
			}
			Role dest = e.getKey();
			List<EAction> as = s.getActions();
			Role src = as.get(0).peer;
			return isPendingRequest(src, dest)
					&& !as.contains(
								 ((AssrtCoreEPendingRequest) this.Q.get(dest).get(src)).getMsg()
								.toTrueAssertion().toDual(src)
							);
		});*/
	}
}
