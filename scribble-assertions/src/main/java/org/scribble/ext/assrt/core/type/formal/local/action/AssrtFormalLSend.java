package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;

public class AssrtFormalLSend extends AssrtFormalLComm
{
	public final Role receiver;

	public AssrtFormalLSend(Role receiver, AssrtMsg msg, List<AssrtMsg> consumed)
	{
		super(receiver, msg, consumed);
		this.receiver = receiver;
	}

	public AssrtFormalLSend(Role receiver, AssrtMsg msg, List<AssrtMsg> consumed,
							LinkedHashMap<AssrtVar, AssrtAFormula> updates) {
		super(receiver, msg, consumed, updates);
		this.receiver = receiver;
	}

	@Override
	public AssrtFormalLSend prependSilent(AssrtMsg m) {
		List<AssrtMsg> ms = new LinkedList<>(this.silent);
		ms.add(0, m);
		return AssrtFormalLFactory.factory.send(this.receiver, this.msg, ms);
	}

	@Override
	public AssrtFormalLSend drop() {
		return AssrtFormalLFactory.factory.send(this.receiver, this.msg);
	}

	@Override
	public AssrtFormalLSend addStateUpdate(AssrtVar v, AssrtAFormula e) {
		LinkedHashMap<AssrtVar, AssrtAFormula> tmp = new LinkedHashMap<>(this.updates);
		tmp.put(v, e);
		return new AssrtFormalLSend(this.receiver, this.msg, this.silent, tmp);
	}

	@Override
	public String getCommSymbol() {
		return "!";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.SEND_HASH;
		hash = 31 * hash + super.hashCode();  // Includes receiver
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalLSend))
		{
			return false;
		}
		return super.equals(o);
	}

	//public abstract boolean canEquals(Object o);
}
