package org.scribble.ext.assrt.core.type.session.local;

import java.util.LinkedHashMap;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.session.AssrtChoice;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

public class AssrtLChoice extends AssrtChoice<Local, AssrtLType>
		implements AssrtLType
{
	public final Role peer;  // this.peer == super.role
	
	protected AssrtLChoice(CommonTree source, Role peer,
			AssrtLActionKind kind,
			LinkedHashMap<AssrtMsg, AssrtLType> cases)
	{
		super(source, peer, kind, cases);
		this.peer = peer;
	}
	
	@Override
	public AssrtLActionKind getKind()
	{
		return (AssrtLActionKind) super.kind;
	}

	@Override
	public String toString()
	{
		return this.role.toString() + this.kind + casesToString();
	}
	
	@Override
	public int hashCode()
	{
		int hash = 2399;
		hash = 31 * hash + super.hashCode();  // Does this.peer/super.role
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtLChoice))
		{
			return false;
		}
		return super.equals(obj);  // Checks canEquals and this.peer/super.role
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtLChoice;
	}
}
