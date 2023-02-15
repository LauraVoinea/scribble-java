package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

// ...but more like a global action? (cf. formal LTS)
public class AssrtFormalLReceive extends AssrtFormalLComm
{
	public final Role sender;

	public AssrtFormalLReceive(Role sender, AssrtMsg msg, List<AssrtMsg> consumed)
	{
		super(sender, msg, consumed);
		this.sender = sender;
	}

	public AssrtFormalLReceive(Role sender, AssrtMsg msg, List<AssrtMsg> consumed,
							   LinkedHashMap<AssrtVar, AssrtAFormula> updates)
	{
		super(sender, msg, consumed, updates);
		this.sender = sender;
	}

	@Override
	public AssrtFormalLReceive prependSilent(AssrtMsg m) {
		List<AssrtMsg> ms = new LinkedList<>(this.silent);
		ms.add(0, m);
		return AssrtFormalLFactory.factory.receive(this.sender, this.msg, ms);
	}

	@Override
	public AssrtFormalLReceive drop() {
		return AssrtFormalLFactory.factory.receive(this.sender, this.msg);
	}

	@Override
	public AssrtFormalLReceive addStateUpdate(AssrtVar v, AssrtAFormula e) {
		LinkedHashMap<AssrtVar, AssrtAFormula> tmp = new LinkedHashMap<>(this.updates);
		tmp.put(v, e);
		return new AssrtFormalLReceive(this.sender, this.msg, this.silent, tmp);
	}

	@Override
	public String getCommSymbol() {
		return "?";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.RECEIVE_HASH;
		hash = 31 * hash + super.hashCode();  // Includes sender
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
}
