package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLEpsilon;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;

public class AssrtFormalLSilent extends AssrtFormalTypeBase
		implements AssrtFormalLocal {

	public final Map<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases;

	// Pre: cases.size() > 1
	protected AssrtFormalLSilent(LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases) {
		this.cases = Collections.unmodifiableMap(new LinkedHashMap<>(cases));
	}

	@Override
	public Set<AssrtLAction> getSteppable(AssrtLambda lambda) {
		LinkedHashSet<AssrtLAction> res = new LinkedHashSet<>();
		for (Pair<AssrtMsg, AssrtFormalLocal> x : this.cases.values()) {
			List<AssrtAnnotDataName> pay = x.left.pay;
			if (pay.size() != 1) {
				throw new RuntimeException("Shouldn't get here: " + pay);
			}
			AssrtAnnotDataName d = pay.get(0);
			if (lambda.canAdd(d.var, Multiplicity.ZERO, d.data)) {
				res.add(new AssrtLEpsilon(x.left));
			}
		}
		return res;
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLocal>> step(
			AssrtLambda lambda, AssrtLAction a) {
		if (!(a instanceof AssrtLEpsilon)) {
			return Optional.empty();
		}
		AssrtLEpsilon cast = (AssrtLEpsilon) a;
		if (!this.cases.containsKey(cast.msg.op)) {
			return Optional.empty();
		}
		List<AssrtAnnotDataName> pay = cast.msg.pay;
		if (pay.size() != 1) {
			throw new RuntimeException("TODO " + this + " ,, " + a);
		}
		AssrtAnnotDataName d = pay.get(0);
		Optional<AssrtLambda> add = lambda.add(d.var, Multiplicity.ZERO, d.data);
		if (!add.isPresent()) {
			return Optional.empty();
		}
		return Optional.of(new Pair<>(add.get(), this.cases.get(cast.msg.op).right));
	}

	@Override
	public Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda, AssrtRho rho) {
		LinkedHashSet<AssrtLAction> res = new LinkedHashSet();
		for (AssrtLAction a : getSteppable(lambda)) {

			// XXX HERE HERE do translation of locals to formals and test branch/select steps and derived steps, then do silents

			if (a instanceof AssrtLEpsilon) {  // Should all be here
				throw new RuntimeException("TODO " + a);
			} else {
				throw new RuntimeException("Shouldn't get here " + a);
			}
		}
		return res;
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLocal, AssrtRho>> dstep(
			AssrtLambda lambda, AssrtRho rho, AssrtLAction a) {
		throw new RuntimeException("Shouldn't get in here: " + this);
	}

	@Override
	public String toString() {
		return AssrtFormalLChoice.casesToString(this.cases);
	}
	
	@Override
	public int hashCode()
	{
		int hash = SILENT_HASH;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.cases.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtFormalLSilent))
		{
			return false;
		}
		AssrtFormalLSilent them = (AssrtFormalLSilent) obj;
		return super.equals(obj)  // Checks canEquals
			&& this.cases.equals(them.cases);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtFormalLSilent;
	}
}
