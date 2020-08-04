package org.scribble.ext.assrt.core.type.session.local;

import org.scribble.core.type.kind.Local;
import org.scribble.ext.assrt.core.type.session.AssrtEnd;


public class AssrtLEnd extends AssrtEnd<Local, AssrtLType> implements AssrtLType
{
	public static final AssrtLEnd END = new AssrtLEnd();
	
	private AssrtLEnd()
	{
		
	}
	
	@Override
	public boolean equals(Object obj)
	{
		if (!(obj instanceof AssrtLEnd))
		{
			return false;
		}
		return super.equals(obj);  // Checks canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtLEnd;
	}

	@Override
	public int hashCode()
	{
		return 31*2383;
	}
}
