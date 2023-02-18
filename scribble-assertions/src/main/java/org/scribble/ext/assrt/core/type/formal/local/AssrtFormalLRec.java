package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLEnter;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.Quadple;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import javax.swing.text.html.Option;
import java.util.*;
import java.util.stream.Collectors;

// !!! Don't really need to separate silent and non-silent rec and recvar constructors -- and multiple state vars can be done in one step (cf. comm pay vars), the step is the rec(var), not the svars
public class AssrtFormalLRec extends AssrtFormalTypeBase
		implements AssrtFormalLType
{
	public final RecVar recvar;
	public final AssrtFormalLType body;

	// Multip because combining silent and non-silent cases
	public final Map<AssrtVar, Triple<Multiplicity, DataName, AssrtAFormula>> statevars;  // var -> (multip, pay, init exprs) -- exprs null for silent rec (multip == Multiplicity.ZERO) -- !!! type is probably redundant to record
	public final AssrtBFormula assertion;  // consolidated refinement

	protected AssrtFormalLRec(RecVar recvar, AssrtFormalLType body,
							  LinkedHashMap<AssrtVar, Triple<Multiplicity, DataName, AssrtAFormula>> svars,
							  AssrtBFormula ass)
	{
		this.recvar = recvar;
		this.statevars = Collections.unmodifiableMap(new LinkedHashMap<>(svars));
		this.body = body;
		this.assertion = ass;
	}

	@Override
	public Set<AssrtFormalLAction> getFormalSteppable(AssrtLambda lambda) {
		throw new RuntimeException("TODO: based on unbounded unfolding");
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLType>> fstep(AssrtLambda lambda, AssrtFormalLAction a) {
		throw new RuntimeException("TODO: based on unbounded unfolding");
	}

	@Override
	public Set<AssrtFormalLAction> getIntermedSteppable(
			AssrtLambda lambda, AssrtRho rho) {
		AssrtFormalLFactory lf = AssrtFormalLFactory.factory;
		/*if (this.statevars.size() != 1) {
			throw new RuntimeException("TODO " + this);
		}*/
		if (rho.map.containsKey(this.recvar)) {
			return Collections.emptySet();
		}

		Map<AssrtVar, Quadple<Multiplicity, DataName, AssrtBFormula, AssrtAFormula>> svars = new LinkedHashMap<>();
		for (Map.Entry<AssrtVar, Triple<Multiplicity, DataName, AssrtAFormula>> es :
				this.statevars.entrySet()) {
			AssrtVar svar = es.getKey();
			if (lambda.map.containsKey(svar)) {  // !!! Lambda , Lambda -- comma is disjoint
				return Collections.emptySet();  // If one seen, all should be seen...
			}

			Triple<Multiplicity, DataName, AssrtAFormula> p = es.getValue();
			svars.put(svar, new Quadple<>(p.left, p.middle, this.assertion, p.right));  // !!! duplicating assertion
		}

		HashSet<AssrtFormalLAction> res = new HashSet<>();

		//res.add(lf.enter(this.recvar, svar, p.left, p.middle, p.right, this.assertion));
		res.add(lf.enter(this.recvar, svars));

		return res;
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(
			AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho) {
		if (!(a instanceof AssrtFormalLEnter)) {
			throw new RuntimeException("Shouldn't get here: " + a);
		}
		if (rho.map.containsKey(this.recvar)) {  // ??? should be same for lambda (also in branch/select?)
			throw new RuntimeException("TODO " + this);
		}
		AssrtFormalLEnter cast = (AssrtFormalLEnter) a;

		/*if (this.statevars.size() != 1) {
			throw new RuntimeException("TODO " + this);
		}
		AssrtVar svar = this.statevars.keySet().iterator().next();
		if (lambda.map.containsKey(svar)) {
			return Optional.empty();
		}*/
		if (this.statevars.keySet().stream().anyMatch(x -> !cast.svars.containsKey(x))) {
			return Optional.empty();
		}

		System.out.println("4444444: " + this + " ,, " + cast);

		// TODO combine with above
		if (this.statevars.keySet().stream().anyMatch(x -> {
			Triple<Multiplicity, DataName, AssrtAFormula> t3 = this.statevars.get(x);
			Quadple<Multiplicity, DataName, AssrtBFormula, AssrtAFormula> t4 = cast.svars.get(x);
			if (!t3.left.equals(t4.fst) || !t3.middle.equals(t4.snd) || !Objects.equals(t3.right, t4.fth) || !this.assertion.equals(t4.thd)) {
				return true;
			}
			return false;
		})) {
			return Optional.empty();
		}

		// init not really used in LTS -- no "loop counting" -- so could collapse silent and non-silent labels/rules (for RCA building, not bisim)

		//...update lambda and rho, and proceed with body
		Optional<AssrtLambda> opt = Optional.of(lambda);
		for (Map.Entry<AssrtVar, Quadple<Multiplicity, DataName, AssrtBFormula, AssrtAFormula>> e
				: cast.svars.entrySet()) {
			Quadple<Multiplicity, DataName, AssrtBFormula, AssrtAFormula> q = e.getValue();
			opt = opt.flatMap(x -> x.add(e.getKey(), q.fst, q.snd));
		}
		if (!opt.isPresent()) {
			return Optional.empty();
		}
		AssrtLambda lam1 = opt.get();
		Optional<AssrtRho> rhoAdd = rho.add(this.recvar, lam1, this.body);
		if (!rhoAdd.isPresent()) {
			return Optional.empty();
		}
		AssrtRho rho1 = rhoAdd.get();

		return Optional.of(new Triple<>(lam1, this.body, rho1));
	}

	@Override
	public Set<AssrtFormalLAction> getExplicitSteppable(AssrtLambda lambda, AssrtRho rho) {
		return getIntermedSteppable(lambda, rho);
	}

	@Override
	public Set<Pair<AssrtLambda, AssrtFormalLType>> fastforwardEnters(AssrtLambda lambda, AssrtRho rho) {
		Set<AssrtFormalLAction> imeds = getIntermedSteppable(lambda, rho);
		if (imeds.size() != 1) { // Cf. getIntermedSteppable returns singleton
			throw new RuntimeException("Shouldn't get in here");
		}
		AssrtFormalLAction a = imeds.iterator().next();
		Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> opt = istep(lambda, a, rho);
		if (!opt.isPresent()) {
			throw new RuntimeException("Shouldn't get in here");
		}
		Triple<AssrtLambda, AssrtFormalLType, AssrtRho> succ = opt.get();
		return succ.middle.fastforwardEnters(succ.left, succ.right);
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> estep(
			AssrtLambda lambda, AssrtRho rho, AssrtFormalLAction a) {
		return istep(lambda, a, rho);
	}

	@Override
	public String toString() {
		return "mu " + this.recvar + "("
				+ this.statevars.entrySet().stream()
						.map(x -> {
							Triple<Multiplicity, DataName, AssrtAFormula> p = x.getValue();
							return x.getKey() + "^" + p.left + ":" + p.middle +
									(p.right == null ? "" : " := " + p.right);
						}).collect(Collectors.joining(", "))
				+ "){" + this.assertion + "}." + this.body;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalLRec))
		{
			return false;
		}
		AssrtFormalLRec them = (AssrtFormalLRec) o;
		return super.equals(o)  // Checks canEquals -- implicitly checks kind
				&& this.recvar.equals(them.recvar)
				&& this.body.equals(them.body)
				&& this.statevars.equals(them.statevars)
				&& this.assertion.equals(them.assertion);
	}
	
	@Override
	public boolean canEquals(Object o) {
		return o instanceof AssrtFormalLRec;
	}
	
	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.REC_HASH;
		hash = 31 * hash + this.recvar.hashCode();
		hash = 31 * hash + this.body.hashCode();
		hash = 31 * hash + this.statevars.hashCode();
		hash = 31 * hash + this.assertion.hashCode();
		return hash;
	}
}
