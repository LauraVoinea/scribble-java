package org.scribble.ext.assrt.core.type.formula;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

// TODO: refactor, not just "A(rith)" anymore, e.g., String sort
public abstract class AssrtAVarFormula extends AssrtAFormula
{
	public final String name; 

	protected AssrtAVarFormula(String name)
	{
		this.name = name; 
	}
	
	// i.e., to "type"
	public abstract //AssrtPayElemType<?> 
	AssrtVar toName();

	@Override
	public AssrtAVarFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		return this.equals(old) ? neu : this;
	}

	@Override
	public boolean isConstant()
	{
		return false;
	}
		
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		/*if (this.name.startsWith("_dum"))  // CHECKME
		{
			throw new RuntimeException("[assrt] Use squash first: " + this);
		}*/
		//return "(" + this.name + ")";
		return this.name;
	}
	
	@Override
	public Set<AssrtVar> getIntVars()
	{
		Set<AssrtVar> vars = new HashSet<>();
		vars.add(toName());  // TODO: currently may also be a role
		return vars; 
	}
	
	@Override
	public String toString()
	{
		return this.name; 
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtVarFormula))
		{
			return false;
		}
		return super.equals(this)  // Does canEqual
				&& this.name.equals(((AssrtVarFormula) o).name);
	}

	@Override
	public int hashCode()
	{
		int hash = 9461;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.name.hashCode();
		return hash;
	}
}
