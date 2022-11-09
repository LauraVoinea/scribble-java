package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLChoice;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLocal;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class AssrtFormalGChoice extends AssrtFormalTypeBase
		implements AssrtFormalGlobal {

	public final Role sender;
	public final Role receiver;
	public final Map<Op, Pair<AssrtMsg, AssrtFormalGlobal>> cases;  // Invariant: op.equals(assrtMsg)

	// Pre: cases.size() > 1
	protected AssrtFormalGChoice(Role sender, Role receiver,
								 LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGlobal>> cases) {
		this.sender = sender;
		this.receiver = receiver;
		this.cases = Collections.unmodifiableMap(new LinkedHashMap<>(cases));
	}

	@Override
	public AssrtFormalLocal project(AssrtFormalLFactory lf, Role r) {
		LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases =
				this.cases.entrySet().stream().collect(Collectors.toMap(
					x -> x.getKey(),
					x -> {
						Pair<AssrtMsg, AssrtFormalGlobal> v = x.getValue();
						return new Pair<>(v.left, v.right.project(lf, r));
					},
					(x, y) -> null,
					LinkedHashMap::new
			));
		if (this.sender.equals(r)) {
			return lf.select(this.receiver, cases);
		} else if (this.receiver.equals(r)) {
			return lf.branch(this.sender, cases);
		} else {
			return lf.silent(cases);
		}
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
		return this.sender + " -> " + this.receiver + " " +
				AssrtFormalGChoice.casesToString(this.cases);
	}
	
	@Override
	public int hashCode()
	{
		int hash = CHOICE_HASH;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.sender.hashCode();
		hash = 31 * hash + this.receiver.hashCode();
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
		if (!(obj instanceof AssrtFormalGChoice))
		{
			return false;
		}
		AssrtFormalGChoice them = (AssrtFormalGChoice) obj;
		return super.equals(obj)  // Checks canEquals
			&& this.sender.equals(them.sender)
			&& this.receiver.equals(them.receiver)
			&& this.cases.equals(them.cases);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtFormalGChoice;
	}

	// Duplicated from AssrtFormalLChoice
	protected static String casesToString(Map<Op, Pair<AssrtMsg, AssrtFormalGlobal>> cases) {
		String m = cases.values().stream()
				.map(e -> msgToString(e.left) + "." + e.right)
				.collect(Collectors.joining(", "));
		m = cases.size() > 1
				? "{ " + m + " }"
				: ": " + m;
		return m;
	}

	protected static String msgToString(AssrtMsg m) {
		return m.op + "(" +
				m.pay.stream().map(x -> x.var + ":" + x.data)
						.collect(Collectors.joining(", ")) +
				"){" + m.ass + "}";
	}
}
