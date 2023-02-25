package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLEpsilon;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLReceive;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.util.Pair;

import java.util.*;

public class AssrtFormalLBranch extends AssrtFormalLChoice {

	// Pre: cases.size() > 1
	protected AssrtFormalLBranch(Role peer, LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		super(peer, AssrtLActionKind.RECV, cases);
	}

	@Override
	public Set<AssrtFormalLAction> getFormalSteppable(AssrtLambda lambda) {
		AssrtFormalLFactory lf = AssrtFormalLFactory.factory;
		LinkedHashSet<AssrtFormalLAction> res = new LinkedHashSet<>();
		for (Pair<AssrtMsg, AssrtFormalLType> x : this.cases.values()) {
			List<AssrtAnnotDataName> pay = x.left.pay;
			int size = pay.size();
			/*if (size > 1) {
				throw new RuntimeException("Shouldn't get here: " + x.left);
			}
			else if (size == 1) {*/
			if (size >= 1) {  // Now probably subsumes 0 case below
				/*AssrtAnnotDataName d = pay.get(0);
				if (lambda.canAdd(d.var, Multiplicity.OMEGA, d.data)) {  // !!! CHECKME OMEGA or ZERO ?
					res.add(lf.receive(this.peer, x.left));
				}*/
				Optional<AssrtLambda> tmp = Optional.of(lambda);
				for (AssrtAnnotDataName d : pay) {
					tmp = tmp.get().add(d.var, Multiplicity.ZERO, d.data);
					if (!tmp.isPresent()) {
						return Collections.emptySet();
					}
				}
				res.add(lf.receive(this.peer, x.left));
			} else { // size == 0
				res.add(lf.receive(this.peer, x.left));
			}
		}
		return res;
	}

	@Override
	public Optional<Pair<AssrtLambda, AssrtFormalLType>> fstep(
			AssrtLambda lambda, AssrtFormalLAction a) {
		if (!(a instanceof AssrtFormalLReceive)) {
			return Optional.empty();
		}
		AssrtFormalLReceive cast = (AssrtFormalLReceive) a;
		if (!cast.silent.isEmpty()) {
			throw new RuntimeException("Shouldn't get here: " + this + " ,, " + a);
		}
		if (!cast.sender.equals(this.peer)
				|| !this.cases.containsKey(cast.msg.op)) {
			return Optional.empty();
		}
		List<AssrtAnnotDataName> pay = cast.msg.pay;
		int size = pay.size();
		/*if (size > 1) {
			throw new RuntimeException("TODO " + this + " ,, " + a);
		} else {*/
			AssrtLambda next = lambda;
			//if (size == 1) {
				/*AssrtAnnotDataName d = pay.get(0);
				Optional<AssrtLambda> add = lambda.add(d.var, Multiplicity.OMEGA, d.data);
				if (!add.isPresent()) {
					return Optional.empty();
				}
				next = add.get();*/
			if (size >= 1) {  // Now probably subsumes 0 case below
				//AssrtAnnotDataName d = pay.get(0);
				for (AssrtAnnotDataName d : pay) {
					Optional<AssrtLambda> add = next.add(d.var, Multiplicity.OMEGA, d.data);
					if (!add.isPresent()) {
						return Optional.empty();
					}
					next = add.get();
				}
			} else {
				next = lambda;
			}
			return Optional.of(new Pair<>(next, this.cases.get(cast.msg.op).right));
		//}
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
