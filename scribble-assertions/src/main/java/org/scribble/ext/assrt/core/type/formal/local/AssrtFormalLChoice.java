package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public abstract class AssrtFormalLChoice extends AssrtFormalTypeBase
		implements AssrtFormalLType {

	public final Role peer;
	public final AssrtLActionKind kind;
	public final Map<Op, Pair<AssrtMsg, AssrtFormalLType>> cases;  // Invariant: op.equals(assrtMsg)

	// Pre: cases.size() > 1
	protected AssrtFormalLChoice(Role peer, AssrtLActionKind kind,
								 LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		this.peer = peer;
		this.kind = kind;
		this.cases = Collections.unmodifiableMap(new LinkedHashMap<>(cases));
	}

	@Override
	public Set<AssrtFormalLAction> getInterSteppable(AssrtLambda lambda, AssrtRho rho) {
		return getSteppable(lambda);
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> istep(
			AssrtLambda lambda, AssrtFormalLAction a, AssrtRho rho) {
		Optional<Pair<AssrtLambda, AssrtFormalLType>> step = step(lambda, a);
		if (!step.isPresent()) {
			return Optional.empty();
		}
		Pair<AssrtLambda, AssrtFormalLType> res = step.get();
		return Optional.of(new Triple<>(res.left, res.right, rho));
	}

	@Override
	public Set<AssrtFormalLAction> getDerivSteppable(AssrtLambda lambda, AssrtRho rho) {
		return getInterSteppable(lambda, rho);
	}

	@Override
	public Optional<Triple<AssrtLambda, AssrtFormalLType, AssrtRho>> dstep(
			AssrtLambda lambda, AssrtRho rho, AssrtFormalLAction a) {
		return istep(lambda, a, rho);
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
				AssrtFormalLChoice.casesToString(this.cases);
	}

	/* */
	
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
		if (!(obj instanceof AssrtFormalLChoice))
		{
			return false;
		}
		AssrtFormalLChoice them = (AssrtFormalLChoice) obj;
		return super.equals(obj)  // Checks canEquals
			&& this.peer.equals(them.peer)
			&& this.kind.equals(them.kind)
			&& this.cases.equals(them.cases);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtFormalLChoice;
	}

	protected static String casesToString(Map<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		String s = cases.values().stream()
				.map(e -> msgToString(e.left) + "." + e.right)
				.collect(Collectors.joining(", "));
		s = (cases.size() > 1)
				? "{ " + s + " }"
				: ":" + s;
		return s;
	}

	protected static String msgToString(AssrtMsg m) {
		return m.op + "(" +
				m.pay.stream().map(x -> x.var + ":" + x.data)
						.collect(Collectors.joining(", ")) +
				"){" + m.ass + "}";
	}
}
