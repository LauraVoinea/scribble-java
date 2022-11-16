package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

// ...but more like a global action? (cf. formal LTS)
public class AssrtFormalLEpsilon implements AssrtFormalLAction
{
	public final AssrtMsg msg;

	public AssrtFormalLEpsilon(AssrtMsg msg)
	{
		this.msg = msg;
	}

	@Override
	public String toString()
	{
		return "eps<" + this.msg + ">";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.EPSILON_HASH;
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
		if (!(o instanceof AssrtFormalLEpsilon))
		{
			return false;
		}
		AssrtFormalLEpsilon them = (AssrtFormalLEpsilon) o;
		return //them.canEquals(this) &&
			this.msg.equals(them.msg);
	}

	//public abstract boolean canEquals(Object o);
}
