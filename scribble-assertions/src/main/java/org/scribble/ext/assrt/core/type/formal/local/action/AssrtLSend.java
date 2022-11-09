package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLocal;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

public class AssrtLSend extends AssrtLComm
{
	public final Role receiver;

	public AssrtLSend(Role receiver, AssrtMsg msg)
	{
		this(receiver, msg, Collections.emptyList());
	}

	public AssrtLSend(Role receiver, AssrtMsg msg, List<AssrtMsg> consumed)
	{
		super(receiver, msg, consumed);
		this.receiver = receiver;
	}

	@Override
	public AssrtLSend prepend(AssrtMsg m) {
		List<AssrtMsg> ms = new LinkedList<>(this.consumed);
		ms.add(0, m);
		return AssrtFormalLFactory.factory.send(this.receiver, this.msg, ms);
	}

	@Override
	public AssrtLSend drop() {
		return AssrtFormalLFactory.factory.send(this.receiver, this.msg);
	}

	@Override
	public String getCommSymbol() {
		return "!";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLocal.SEND_HASH;
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
		if (!(o instanceof AssrtLSend))
		{
			return false;
		}
		return super.equals(o);
	}

	//public abstract boolean canEquals(Object o);
}
