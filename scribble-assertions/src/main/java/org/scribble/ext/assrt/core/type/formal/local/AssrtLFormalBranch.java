package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLReceive;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLTransfer;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

public abstract class AssrtLFormalBranch extends AssrtLFormalChoice {

	// Pre: cases.size() > 1
	protected AssrtLFormalBranch(Role peer, AssrtLActionKind kind,
                                 LinkedHashMap<Op, Pair<AssrtMsg, AssrtLFormal>> cases) {
		super(peer, kind, cases);
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtLFormal>> step(
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
	public Set<AssrtLAction> getSteppable() {
		return this.cases.values().stream()
				.map(x -> new AssrtLReceive(this.peer, x.left))
				.collect(Collectors.toSet());
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtLFormal, Rho>> dstep(
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
		if (!(obj instanceof AssrtLFormalBranch))
		{
			return false;
		}
		AssrtLFormalBranch them = (AssrtLFormalBranch) obj;
		return super.equals(obj);  // Checks canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtLFormalBranch;
	}
}
