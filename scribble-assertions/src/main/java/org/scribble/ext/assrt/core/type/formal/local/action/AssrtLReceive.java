package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtLFormal;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

// ...but more like a global action? (cf. formal LTS)
public class AssrtLReceive implements AssrtLAction
{
	public final Role sender;
	public final AssrtMsg msg;

	public AssrtLReceive(Role sender, AssrtMsg msg)
	{
		this.sender = sender;
		this.msg = msg;
	}

	@Override
	public String toString()
	{
		return this.sender + "?" + this.msg;
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtLFormal.RECEIVE_HASH;
		hash = 31 * hash + this.sender.hashCode();
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
		if (!(o instanceof AssrtLReceive))
		{
			return false;
		}
		AssrtLReceive them = (AssrtLReceive) o;
		return //them.canEquals(this) &&
			this.sender.equals(them.sender)
					&& this.msg.equals(them.msg);
	}

	//public abstract boolean canEquals(Object o);
}
