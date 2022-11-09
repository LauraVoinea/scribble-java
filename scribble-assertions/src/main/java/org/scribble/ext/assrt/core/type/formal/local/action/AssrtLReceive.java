package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLocal;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

// ...but more like a global action? (cf. formal LTS)
public class AssrtLReceive extends AssrtLComm
{
	public final Role sender;

	public AssrtLReceive(Role sender, AssrtMsg msg)
	{
		this(sender, msg, Collections.emptyList());
	}

	public AssrtLReceive(Role sender, AssrtMsg msg, List<AssrtMsg> consumed)
	{
		super(sender, msg, consumed);
		this.sender = sender;
	}

	@Override
	public AssrtLReceive prepend(AssrtMsg m) {
		List<AssrtMsg> ms = new LinkedList<>(this.consumed);
		ms.add(0, m);
		return AssrtFormalLFactory.factory.receive(this.sender, this.msg, ms);
	}

	@Override
	public AssrtLReceive drop() {
		return AssrtFormalLFactory.factory.receive(this.sender, this.msg);
	}

	@Override
	public String getCommSymbol() {
		return "?";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLocal.RECEIVE_HASH;
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
		if (!(o instanceof AssrtLSend))
		{
			return false;
		}
		return super.equals(o);
	}
}
