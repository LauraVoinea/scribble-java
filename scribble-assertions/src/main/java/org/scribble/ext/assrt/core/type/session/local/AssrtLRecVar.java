package org.scribble.ext.assrt.core.type.session.local;

import java.util.List;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.session.AssrtRecVar;

	
public class AssrtLRecVar extends AssrtRecVar<Local, AssrtLType>
		implements AssrtLType
{
	protected AssrtLRecVar(CommonTree source, RecVar var,
			List<AssrtAFormula> annotexprs)
	{
		super(source, var, annotexprs);
	}

	@Override
	public boolean equals(Object obj)
	{
		if (!(obj instanceof AssrtLRecVar))
		{
			return false;
		}
		return super.equals(obj);  // Does canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtLRecVar;
	}

	@Override
	public int hashCode()
	{
		int hash = 2417;
		hash = 31*hash + super.hashCode();
		return hash;
	}
}
