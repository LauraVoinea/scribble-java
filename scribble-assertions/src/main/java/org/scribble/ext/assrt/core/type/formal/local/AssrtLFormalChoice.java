package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLTransfer;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AssrtLFormalChoice extends AssrtFormalTypeBase
		implements AssrtLFormal {

	public final Role peer;
	public final AssrtLActionKind kind;
	public final Map<Op, Pair<AssrtMsg, AssrtLFormal>> cases;  // Invariant: op.equals(assrtMsg)

	// Pre: cases.size() > 1
	protected AssrtLFormalChoice(Role peer, AssrtLActionKind kind,
								 LinkedHashMap<Op, Pair<AssrtMsg, AssrtLFormal>> cases) {
		this.peer = peer;
		this.kind = kind;
		this.cases = Collections.unmodifiableMap(new LinkedHashMap<>(cases));
	}

	/*@Override
	public Optional<Pair<AssrtLambda, AssrtLFormal>> step(
			AssrtLambda lambda, AssrtLAction a) {
		if (!(a instanceof AssrtLTransfer)) {
			return Optional.empty();
		}
		AssrtLTransfer cast = (AssrtLTransfer) a;
		if (!cast.receiver.equals(this.peer)
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
				.map(x -> new AssrtLTransfer(null, this.peer, x.left))
				.collect(Collectors.toSet());
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtLFormal, Rho>> dstep(
			AssrtLambda lambda, Rho rho, AssrtLAction a) {

		throw new RuntimeException("TODO");

	}*/

	@Override
	public String toString() {
		return this.peer.toString() + this.kind +
				AssrtLFormalChoice.casesToString(this.cases);
	}
	
	@Override
	public int hashCode()
	{
		int hash = CHOICE_HASH;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.peer.hashCode();
		hash = 31 * hash + this.kind.hashCode();
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
		if (!(obj instanceof AssrtLFormalChoice))
		{
			return false;
		}
		AssrtLFormalChoice them = (AssrtLFormalChoice) obj;
		return super.equals(obj)  // Checks canEquals
			&& this.peer.equals(them.peer)
			&& this.kind.equals(them.kind)
			&& this.cases.equals(them.cases);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtLFormalChoice;
	}

	protected static String casesToString(Map<Op, Pair<AssrtMsg, AssrtLFormal>> cases) {
		String s = cases.values().stream()
				.map(e -> e.left + "." + e.right)
				.collect(Collectors.joining(", "));
		s = (cases.size() > 1)
				? "{ " + s + " }"
				: ":" + s;
		return s;
	}
}
