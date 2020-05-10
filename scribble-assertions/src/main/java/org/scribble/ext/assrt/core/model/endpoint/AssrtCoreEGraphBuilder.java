package org.scribble.ext.assrt.core.model.endpoint;

import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.scribble.core.model.endpoint.EGraph;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.PayElemType;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.Continue;
import org.scribble.core.type.session.DirectedInteraction;
import org.scribble.core.type.session.DisconnectAction;
import org.scribble.core.type.session.Payload;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.local.LSeq;
import org.scribble.core.visit.local.EGraphBuilder;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.kind.AssrtAnnotDataKind;
import org.scribble.ext.assrt.core.type.name.AssrtIntVar;
import org.scribble.ext.assrt.core.type.session.AssrtCoreMsg;
import org.scribble.ext.assrt.core.type.session.AssrtCoreRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLActionKind;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLChoice;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLEnd;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLRec;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLType;
import org.scribble.ext.assrt.model.endpoint.AssrtEGraphBuilderUtil;
import org.scribble.ext.assrt.model.endpoint.AssrtEState;

public class AssrtCoreEGraphBuilder extends EGraphBuilder
{
	// CHECKME: for convenience, shadowing supers -- OK? or bad
	private final AssrtCore core;
	private final AssrtEGraphBuilderUtil util;  // Not using any features for unguarded choice/recursion/continue (recursion manually tracked here)*/
	
	public AssrtCoreEGraphBuilder(AssrtCore core)
	{
		super(core);
		/*this.core = core;
		this.util = (AssrtEGraphBuilderUtil) this.core.config.mf.local
				.EGraphBuilderUtil();*/
		this.core = core;
		this.util = (AssrtEGraphBuilderUtil) super.util;
	}
	
	// Not LProtocol arg, LProjection currently not subtype of LProtocol
	public EGraph build(LinkedHashMap<AssrtIntVar, AssrtAFormula> svars,
			AssrtBFormula ass, AssrtCoreLType lt,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom)  // Phantoms from init state
	{
		this.util.setEntry(((AssrtCoreEModelFactory) this.core.config.mf.local)
				.newAssrtEState(Collections.emptySet(), svars, ass, phantom));  // "Working" phantoms -- carry over from states to edges
		
		build(lt, this.util.getEntry(), this.util.getExit(), new HashMap<>(),
				new LinkedHashMap<>(), AssrtTrueFormula.TRUE);
		return this.util.finalise();
	}
	
	private void build(AssrtCoreLType lt, AssrtEState s1, AssrtEState s2,
			Map<RecVar, AssrtEState> recs,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom, AssrtBFormula phantAss)  // "Working" phantoms -- carry over from states to edges
	{
		if (lt instanceof AssrtCoreLChoice)
		{
			AssrtCoreLChoice lc = (AssrtCoreLChoice) lt;
			AssrtCoreLActionKind k = lc.getKind();
			lc.cases.entrySet().stream()
					.forEach(e -> buildEdgeAndContinuation(s1, s2, recs, lc.role, k,
							e.getKey(), e.getValue(), phantom, phantAss));
		}
		else if (lt instanceof AssrtCoreLRec)
		{
			AssrtCoreLRec lr = (AssrtCoreLRec) lt;

			//this.util.addAnnotVarInits(s1, Stream.of(lr.annot).collect(Collectors.toMap(a -> a, a -> lr.init)));
			this.util.addStateVars(s1, lr.statevars, lr.assertion);
			this.util.addPhantoms(s1, lr.phantom);

			Map<RecVar, AssrtEState> tmp = new HashMap<>(recs);
			tmp.put(lr.recvar, s1);
			LinkedHashMap<AssrtIntVar, AssrtAFormula> tmp2 = new LinkedHashMap<>(
					phantom);
			//tmp2.putAll(lr.phantom);

			AssrtBFormula tmp3 = phantAss;
			/*if (!lr.assertion.equals(AssrtTrueFormula.TRUE))
			{
				tmp3 = phantAss.equals(AssrtTrueFormula.TRUE)
						? lr.assertion
						: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And,
								phantAss, lr.assertion);
			}*/

			//tmp3 = AssrtTrueFormula.TRUE;  // TODO FIXME -- getFireable currently depends on syntactic assertion matching so cannot "asymmetrically" modify assertions, cf. AssrtCoreSSingleBuffers.canReceive
			// ...TODO: add phantoms in model construction -- no vars are ever unknown? but cannot validate, e.g., x = phantom?

			build(lr.body, s1, s2, tmp, tmp2, tmp3);
		}
		else
		{
			throw new RuntimeException("[assrt-core] Shouldn't get in here: " + lt);
		}
	}

	private void buildEdgeAndContinuation(AssrtEState s1, AssrtEState s2,
			Map<RecVar, AssrtEState> recs, Role r, AssrtCoreLActionKind k,
			AssrtCoreMsg a, AssrtCoreLType cont,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom, AssrtBFormula phantAss)
	{
		if (cont instanceof AssrtCoreLEnd)
		{
			this.util.addEdge(s1, toEAction(r, k, a, phantom, phantAss), s2);
		}
		else if (cont instanceof AssrtCoreRecVar)
		{
			AssrtCoreLRecVar rv = (AssrtCoreLRecVar) cont;
			AssrtEState s = recs.get(rv.recvar);

			//AssrtArithFormula expr = crv.annotexprs;
			//AssrtDataTypeVar annot = s.getAnnotVars().keySet().iterator().next();
			List<AssrtAFormula> annotexprs = rv.stateexprs;
			//List<AssrtDataTypeVar> annotvars = s.getAnnotVars().keySet().stream().collect(Collectors.toList());

			this.util.addEdge(s1, toEAction(r, k, a, //annotvars,
					annotexprs, phantom, phantAss), s);
		}
		else
		{
			AssrtEState s = (AssrtEState) ((AssrtCoreEModelFactory) this.util.mf.local)
					.EState(Collections.emptySet());  // statevars added by upcoming build
					// FIXME: call Assrt directly? -- no "vars" here though (intermediate sequence states), only on rec states

			EAction tmp;
			if (cont instanceof AssrtCoreLRec)  // `a` is a f/w rec -- cont-rec `a` handled by RecVar `cont` above
			{
				// Cf. AssrtCoreGDo.inline, f/w rec statevar expr inlining
				LinkedHashMap<AssrtIntVar, AssrtAFormula> svars = ((AssrtCoreLRec) cont).statevars;
				tmp = toEAction(r, k, a, new LinkedList<>(svars.values()),
						phantom, phantAss);
			}
			else
			{
				tmp = toEAction(r, k, a, phantom, phantAss);
			}
			this.util.addEdge(s1, tmp, s);
			build(cont, s, s2, recs, new LinkedHashMap<>(), AssrtTrueFormula.TRUE);  // phantoms cleared
		}
	}
	
	private EAction toEAction(Role r, AssrtCoreLActionKind k, AssrtCoreMsg a,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom, AssrtBFormula phantAss)  // Carried over from state
	{
		//return toEAction(r, k, a, AssrtCoreESend.DUMMY_VAR, AssrtCoreESend.ZERO);
		return toEAction(r, k, a, Collections.emptyList(), phantom, phantAss);
	}

	/*// TODO FIXME: need access to data decls -- in general, need to extend Core with this info (n.b. job is not visible from core)
	private Payload convertPayToExtNames(AssrtCoreMsg m)
	{
		List<AssrtAnnotDataName> es = new LinkedList<>();
		for (AssrtAnnotDataName e : m.pay) {
			if (e.data.toString.equals("...String..."))  // 
		}
		new Payload(
				m.pay.stream().map(p -> (PayElemType<AssrtAnnotDataKind>) p)
						.collect(Collectors.toList()));
	}
	*/

	private EAction toEAction(Role r, AssrtCoreLActionKind k, AssrtCoreMsg a,
			//AssrtDataTypeVar annot, AssrtArithFormula expr)
			List<AssrtAFormula> annotexprs,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom, AssrtBFormula phantAss)  // Carried over from state
	{
		AssrtCoreEModelFactory ef = (AssrtCoreEModelFactory) this.util.mf.local;  // FIXME: factor out
		/*AssrtBFormula ass = a.ass;
		if (!phantAss.equals(AssrtTrueFormula.TRUE))
		{
			ass = ass.equals(AssrtTrueFormula.TRUE)
					? phantAss
					: AssrtFormulaFactory.AssrtBinBool(AssrtBinBFormula.Op.And, a.ass,
							phantAss);
		}*/
		if (k.equals(AssrtCoreLActionKind.SEND))
		{
			return ef.AssrtCoreESend(r, a.op, 
					//new Payload(Arrays.asList(a.pays)),
					new Payload(
							a.pay.stream().map(p -> (PayElemType<AssrtAnnotDataKind>) p)
									.collect(Collectors.toList())),
					a.ass, //annot,
					annotexprs,
					//phantom, phantAss);
					a.phantom, a.phantAss);

		}
		else if (k.equals(AssrtCoreLActionKind.RECV))
		{
			//return ef.newAssrtEReceive(r, a.op, new Payload(Arrays.asList(a.pay)), a.ass);
			return ef.AssrtCoreERecv(r, a.op,
					//new Payload(Arrays.asList(a.pays)),
					new Payload(
							a.pay.stream().map(p -> (PayElemType<AssrtAnnotDataKind>) p)
									.collect(Collectors.toList())),
					a.ass, //annot,
					annotexprs,
					//phantom, phantAss);
					a.phantom, a.phantAss);

			// CHECKME: local receive assertions -- why needed exactly?  should WF imply receive assertion always true?

		}
		else if (k.equals(AssrtCoreLActionKind.REQ))
		{
			if (!phantom.isEmpty())
			{
				throw new RuntimeException("TODO");
			}
			return ef.AssrtCoreEReq(r, a.op,
					//new Payload(Arrays.asList(a.pays)),
					new Payload(
							a.pay.stream().map(p -> (PayElemType<AssrtAnnotDataKind>) p)
									.collect(Collectors.toList())),
					a.ass, //annot,
					annotexprs);
		}
		else if (k.equals(AssrtCoreLActionKind.ACC))
		{
			if (!phantom.isEmpty())
			{
				throw new RuntimeException("TODO");
			}
			return ef.AssrtCoreEAcc(r, a.op,
					//new Payload(Arrays.asList(a.pays)),
					new Payload(
							a.pay.stream().map(p -> (PayElemType<AssrtAnnotDataKind>) p)
									.collect(Collectors.toList())),
					a.ass, //annot,
					annotexprs);
		}
		/*else if (a instanceof AssrtCoreLDisconnect)
		{
			return this.util.ef.newEDisconnect(a.peer);
		}*/
		else
		{
			throw new RuntimeException("[assrt-core] Shouldn't get in here: " + k);
		}
	}

	@Override
	public SType<Local, LSeq> visitContinue(Continue<Local, LSeq> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":");
	}

	@Override
	public SType<Local, LSeq> visitChoice(Choice<Local, LSeq> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":");
	}

	@Override
	public SType<Local, LSeq> visitDirectedInteraction(
			DirectedInteraction<Local, LSeq> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":");
	}

	@Override
	public SType<Local, LSeq> visitDisconnect(DisconnectAction<Local, LSeq> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":");
	}

	@Override
	public SType<Local, LSeq> visitRecursion(Recursion<Local, LSeq> n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":");
	}

	@Override
	public LSeq visitSeq(LSeq n)
	{
		throw new RuntimeException("Deprecated for " + getClass() + ":");
	}
}
