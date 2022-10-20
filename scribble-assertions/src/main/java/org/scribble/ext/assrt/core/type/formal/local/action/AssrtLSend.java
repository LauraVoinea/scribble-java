package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtLFormal;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

// ...but more like a global action? (cf. formal LTS)
public class AssrtLSend implements AssrtLAction
{
	public final Role receiver;
	public final AssrtMsg msg;

	public AssrtLSend(Role receiver, AssrtMsg msg)
	{
		this.receiver = receiver;
		this.msg = msg;
	}

	@Override
	public String toString()
	{
		return this.receiver + "!" + this.msg;
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtLFormal.SEND_HASH;
		hash = 31 * hash + this.receiver.hashCode();
		hash = 31 * hash + this.msg.hashCode();
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
		AssrtLSend them = (AssrtLSend) o;
		return //them.canEquals(this) &&
			this.receiver.equals(them.receiver)
					&& this.msg.equals(them.msg);
	}

	//public abstract boolean canEquals(Object o);
}
