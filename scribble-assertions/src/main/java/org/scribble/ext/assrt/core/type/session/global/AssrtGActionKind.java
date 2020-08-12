package org.scribble.ext.assrt.core.type.session.global;

import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.session.AssrtActionKind;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;

public enum AssrtGActionKind implements AssrtActionKind<Global>
{
	MSG_TRANSFER,
	CONNECT;
	//DISCONNECT
	
	@Override
	public String toString()
	{
		switch (this)
		{
			case MSG_TRANSFER: return "->";
			case CONNECT: return "->>";
			default: throw new RuntimeException("Won't get here: " + this);
		}
	}
	
	// src is global src, subj is either src or dest
	public AssrtLActionKind project(Role src, Role subj)
	{
		return 
				this == AssrtGActionKind.MSG_TRANSFER
				? (src.equals(subj) 
						? AssrtLActionKind.SEND : AssrtLActionKind.RECV)
				: (src.equals(subj) 
						? AssrtLActionKind.REQ : AssrtLActionKind.ACC)
				;
	}
}
