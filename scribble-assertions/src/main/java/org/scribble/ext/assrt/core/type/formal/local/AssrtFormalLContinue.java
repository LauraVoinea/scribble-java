package org.scribble.ext.assrt.core.type.formal.local;

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

public class AssrtFormalLContinue extends AssrtFormalTypeBase
		implements AssrtFormalLType
{
	public final RecVar recvar;

	// Multip because combining silent and non-silent cases
	public final Map<AssrtVar, Pair<Multiplicity, AssrtAFormula>> statevars;  // var -> (multip, init exprs) -- exprs null for silent rec (multip == Multiplicity.ZERO)
	public final AssrtBFormula assertion;  // consolidated refinement

	protected AssrtFormalLContinue(RecVar recvar,
		   LinkedHashMap<AssrtVar, Pair<Multiplicity, AssrtAFormula>> svars, AssrtBFormula ass)
	{
		this.recvar = recvar;
		this.statevars = Collections.unmodifiableMap(new LinkedHashMap<>(svars));
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
		AssrtVar svar = this.statevars.keySet().iterator().next();
		/*if (lambda.map.containsKey(svar)) {  // !!! Lambda , Lambda -- comma is disjoint
			return Collections.emptySet();
		}*/
		if (!rho.map.containsKey(this.recvar)) {
			return Collections.emptySet();
		}
		Pair<Multiplicity, AssrtAFormula> p = this.statevars.get(svar);
		HashSet<AssrtFormalLAction> res = new HashSet<>();
		res.add(lf.unfold(this.recvar, svar, p.left, p.right));
		return res;
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(
			AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho) {
		if (!(a instanceof AssrtFormalLEnter)) {
			throw new RuntimeException("Shouldn't get here: " + a);
		}
		if (!rho.map.containsKey(this.recvar)) {  // ??? should be same for lambda (also in branch/select?)
			return Optional.empty();
		}
		AssrtFormalLEnter cast = (AssrtFormalLEnter) a;
		if (this.statevars.size() != 1) {
			throw new RuntimeException("TODO " + this);
		}
		AssrtVar svar = this.statevars.keySet().iterator().next();
		if (lambda.map.containsKey(svar)) {
			return Optional.empty();
		}
		Pair<Multiplicity, AssrtAFormula> p = this.statevars.get(svar);
		if (!this.recvar.equals(cast.recvar) || !svar.equals(cast.svar)
				|| p.left != cast.multip || !Objects.equals(p.right, cast.init)) {
			return Optional.empty();
		} else {
			Pair<AssrtLambda, AssrtFormalLType> q = rho.map.get(this.recvar);
			return Optional.of(new Triple<>(q.left, q.right, rho));
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
		return this.recvar + "<"
				+ this.statevars.entrySet().stream()
						.map(x -> {
							Pair<Multiplicity, AssrtAFormula> p = x.getValue();
							return x.getKey() + "^" + p.left +
									(p.right == null ? "" : " := " + p.right);
						}).collect(Collectors.joining(", "))
				+ ">";
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalLContinue))
		{
			return false;
		}
		AssrtFormalLContinue them = (AssrtFormalLContinue) o;
		return super.equals(o)  // Checks canEquals -- implicitly checks kind
				&& this.recvar.equals(them.recvar)
				&& this.statevars.equals(them.statevars)
				&& this.assertion.equals(them.assertion);
	}
	
	@Override
	public boolean canEquals(Object o) {
		return o instanceof AssrtFormalLContinue;
	}
	
	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.CONTINUE_HASH;
		hash = 31 * hash + this.recvar.hashCode();
		hash = 31 * hash + this.statevars.hashCode();
		hash = 31 * hash + this.assertion.hashCode();
		return hash;
	}
}
