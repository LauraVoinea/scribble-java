package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;


public class AssrtFormalLEnd implements AssrtFormalLType
{
	public static final AssrtFormalLEnd END = new AssrtFormalLEnd();

	private AssrtFormalLEnd()
	{
		
	}

	@Override
	public Set<AssrtFormalLAction> getFormalSteppable(AssrtLambda lambda) {
		return Collections.emptySet();
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLType>> fstep(AssrtLambda lambda, AssrtFormalLAction a) {
		return Optional.empty();
	}

	@Override
	public Set<AssrtFormalLAction> getIntermedSteppable(AssrtLambda lambda, AssrtRho rho) {
		return getFormalSteppable(lambda);
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(
			AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho) {
		Pair<AssrtLambda, AssrtFormalLType> step = fstep(lambda, a).get();
		return Optional.of(new Triple<>(step.left, step.right, rho));
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
		return "end";
	}

	/* Aux */

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof AssrtFormalLEnd)) {
			return false;
		}
		return super.equals(obj);  // Checks canEquals
	}
	
	@Override
	public boolean canEquals(Object o) {
		return o instanceof AssrtFormalLEnd;
	}

	@Override
	public int hashCode() {
		int hash = AssrtFormalLType.END_HASH;
		hash =  31 * hash;
		return hash;
	}
}
