package org.scribble.ext.assrt.core.type.session.local;

import org.scribble.core.type.kind.Local;
import org.scribble.ext.assrt.core.type.session.AssrtActionKind;

public enum AssrtLActionKind implements AssrtActionKind<Local>
{
	SEND,
	RECV;
	//REQ,
	//ACC;
	
	@Override
	public String toString()
	{
		switch (this)
		{
			case SEND: return "!";
			case RECV: return "?";
			//case REQ:  return "!!";
			//case ACC:  return "??";
			default:   throw new RuntimeException("Won't get here: " + this);
		}
	}
}
