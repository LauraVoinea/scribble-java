package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLReceive;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;

public class AssrtFormalLBranch extends AssrtFormalLChoice {

	// Pre: cases.size() > 1
	protected AssrtFormalLBranch(Role peer, LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases) {
		super(peer, AssrtLActionKind.RECV, cases);
	}

	@Override
	public Set<AssrtLAction> getSteppable(AssrtLambda lambda) {
		return AssrtFormalLSelect.getStepAux(this.cases, lambda);
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLocal>> step(
			AssrtLambda lambda, AssrtLAction a) {
		if (!(a instanceof AssrtLReceive)) {
			return Optional.empty();
		}
		AssrtLReceive cast = (AssrtLReceive) a;
		if (!cast.sender.equals(this.peer)
				|| !this.cases.containsKey(cast.msg.op)) {
			return Optional.empty();
		}
		List<AssrtAnnotDataName> pay = cast.msg.pay;
		if (pay.size() != 1) {
			throw new RuntimeException("TODO " + this + " ,, " + a);
		}
		AssrtAnnotDataName d = pay.get(0);
		Optional<AssrtLambda> add = lambda.add(d.var, Multiplicity.OMEGA, d.data);
		if (!add.isPresent()) {
			return Optional.empty();
		}
		return Optional.of(new Pair<>(add.get(), this.cases.get(cast.msg.op).right));
	}

	@Override
	public Set<AssrtLAction> getDerivSteppable(AssrtLambda lambda) {
		return getSteppable(lambda);
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLocal, Rho>> dstep(
			AssrtLambda lambda, Rho rho, AssrtLAction a) {

		throw new RuntimeException("TODO");

	}

	@Override
	public int hashCode()
	{
		int hash = RECEIVE_HASH;
		hash = 31 * hash + super.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtFormalLBranch))
		{
			return false;
		}
		AssrtFormalLBranch them = (AssrtFormalLBranch) obj;
		return super.equals(obj);  // Checks canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtFormalLBranch;
	}
}
