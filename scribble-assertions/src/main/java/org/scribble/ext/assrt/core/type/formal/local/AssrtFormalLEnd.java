package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.Optional;
import java.util.Set;


public class AssrtFormalLEnd implements AssrtFormalLocal
{
	public static final AssrtFormalLEnd END = new AssrtFormalLEnd();

	private AssrtFormalLEnd()
	{
		
	}

	@Override
	public Set<AssrtLAction> getSteppable(AssrtLambda lambda) {
		return Collections.emptySet();
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLocal>> step(AssrtLambda lambda, AssrtLAction a) {
		return Optional.empty();
	}

	@Override
	public Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda) {
		return Collections.emptySet();
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLocal, Rho>> dstep(AssrtLambda lambda, Rho rho, AssrtLAction a) {
		return Optional.empty();
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
		int hash = AssrtFormalLocal.END_HASH;
		hash =  31 * hash;
		return hash;
	}
}
