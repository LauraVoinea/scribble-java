package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AssrtFormalLComm implements AssrtFormalLAction
{
	public final List<AssrtMsg> silent;  // phantoms, TODO: factor out a "derived action" interface

	public final Role peer;
	public final AssrtMsg msg;

	public AssrtFormalLComm(Role peer, AssrtMsg msg)
	{
		this(peer, msg, Collections.emptyList());
	}

	public AssrtFormalLComm(Role peer, AssrtMsg msg, List<AssrtMsg> silent)
	{
		this.peer = peer;
		this.msg = msg;
		this.silent = silent.stream().collect(Collectors.toList());
	}

	public abstract AssrtFormalLComm prependSilent(AssrtMsg m);
	public abstract AssrtFormalLComm drop();

	@Override
	public String toString()
	{
		return (this.silent.isEmpty() ? "" : this.silent) +
				" " + this.peer + getCommSymbol() + this.msg;
	}

	public abstract String getCommSymbol();

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.COMM_HASH;
		hash = 31 * hash + this.silent.hashCode();
		hash = 31 * hash + this.peer.hashCode();
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
		if (!(o instanceof AssrtFormalLComm))
		{
			return false;
		}
		AssrtFormalLComm them = (AssrtFormalLComm) o;
		return //them.canEquals(this) &&
			this.silent.equals(them.silent) &&
			this.peer.equals(them.peer) && this.msg.equals(them.msg);
	}

	//public abstract boolean canEquals(Object o);
}
