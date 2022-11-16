package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

// ...but more like a global action? (cf. formal LTS)
@Deprecated
public class AssrtLTransfer implements AssrtFormalLAction
{
	public final Role sender;
	public final Role receiver;
	public final AssrtMsg msg;

	public AssrtLTransfer(Role sender, Role receiver, AssrtMsg msg)
	{
		this.sender = sender;
		this.receiver = receiver;
		this.msg = msg;
	}

	@Override
	public String toString()
	{
		return this.sender + "->" + this.receiver + ":" + this.msg;
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.TRANSFER_HASH;
		hash = 31 * hash + this.sender.hashCode();
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
		if (!(o instanceof AssrtLTransfer))
		{
			return false;
		}
		AssrtLTransfer them = (AssrtLTransfer) o;
		return //them.canEquals(this) &&
			this.sender.equals(them.sender)
					&& this.receiver.equals(them.receiver)
					&& this.msg.equals(them.msg);
	}

	//public abstract boolean canEquals(Object o);
}
