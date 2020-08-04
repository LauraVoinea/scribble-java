package org.scribble.ext.assrt.core.type.formula;

import java.util.Map;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

// Variable occurrence
// TODO: integrate with AssrtVar
// CHECKME: currently also used for roles? -- probably need to parse as "ambig"
// and disamb later
public class AssrtVarFormula extends AssrtAVarFormula
{
	// CHECKME: sort field -- cf. AssrtVar

	protected AssrtVarFormula(String name)
	{
		super(name);
	}

	@Override
	public AssrtVarFormula disamb(Map<AssrtVar, DataName> env)
	{
		throw new RuntimeException("Won't get in here: " + this);  // Should not be re-disambiguating 
	}

	// i.e., to "type"
	@Override
	public AssrtVar toName()
	{
		return new AssrtVar(this.name);
	}

	@Override
	public AssrtVarFormula squash()
	{
		return AssrtFormulaFactory.AssrtIntVar(this.name);
	}

	@Override
	public DataName getSort(Map<AssrtVar, DataName> env)
	{
		return env.get(toName());
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
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtVarFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 5903;
		hash = 31 * hash + super.hashCode();
		return hash;
	}
}
