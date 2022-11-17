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
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

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
		if (this.statevars.size() != 1) {
			throw new RuntimeException("TODO " + this);
		}
		if (rho.map.containsKey(this.recvar)) {
			return Collections.emptySet();
		}
		AssrtVar svar = this.statevars.keySet().iterator().next();
		if (lambda.map.containsKey(svar)) {  // !!! Lambda , Lambda -- comma is disjoint
			return Collections.emptySet();
		}
		Triple<Multiplicity, DataName, AssrtAFormula> p = this.statevars.get(svar);
		HashSet<AssrtFormalLAction> res = new HashSet<>();
		res.add(lf.enter(this.recvar, svar, p.left, p.middle, p.right, this.assertion));
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
		if (this.statevars.size() != 1) {
			throw new RuntimeException("TODO " + this);
		}
		AssrtVar svar = this.statevars.keySet().iterator().next();
		if (lambda.map.containsKey(svar)) {
			return Optional.empty();
		}
		Triple<Multiplicity, DataName, AssrtAFormula> p = this.statevars.get(svar);
		if (this.recvar.equals(cast.recvar) && svar.equals(cast.svar) && p.left == cast.multip
				&& p.middle.equals(cast.data) && Objects.equals(p.right, cast.init)) {

			// init not really used in LTS -- no "loop counting" -- so could collapse silent and non-silent labels/rules (for RCA building, not bisim)

			//...update lambda and rho, and proceed with body
			Optional<AssrtLambda> add = lambda.add(svar, cast.multip, cast.data);
			if (!add.isPresent()) {
				return Optional.empty();
			}
			AssrtLambda lam1 = add.get();
			Optional<AssrtRho> rhoAdd = rho.add(this.recvar, lam1, this.body);
			if (!rhoAdd.isPresent()) {
				return Optional.empty();
			}
			AssrtRho rho1 = rhoAdd.get();

			return Optional.of(new Triple<>(lam1, this.body, rho1));
		} else {
			return Optional.empty();
		}
	}

	@Override
	public Set<AssrtFormalLAction> getExplicitSteppable(AssrtLambda lambda, AssrtRho rho) {
		return getIntermedSteppable(lambda, rho);
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
