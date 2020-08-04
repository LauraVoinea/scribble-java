package org.scribble.ext.assrt.core.type.formula;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula;

// String literal
// TODO: factor out with Int -- record "type" as a field
public class AssrtStrValFormula extends AssrtAFormula
{
	public final String val;

	protected AssrtStrValFormula(String s)
	{
		this.val = s;
	}

	@Override
	public AssrtStrValFormula disamb(Map<AssrtVar, DataName> env)
	{
		return this;
	}

	@Override
	public AssrtStrValFormula squash()
	{
		return AssrtFormulaFactory.AssrtStrVal(this.val);
	}

	@Override
	public AssrtStrValFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)  // FIXME: mismatch between Str and ArithFormula
	{
		return this;
	}

	@Override
	public DataName getSort(Map<AssrtVar, DataName> env)
	{
		return new DataName("String");  // TODO: factor out constant
	}

	@Override
	public boolean isConstant()
	{
		return true;
	}
		
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		return "\"" + this.val + "\"";
	}
	
	@Override
	public IntegerFormula toJavaSmtFormula()
	{
		throw new RuntimeException("Deprecated");
	}
	
	@Override
	public Set<AssrtVar> getIntVars()
	{
		return Collections.emptySet();	
	}
	
	@Override
	public String toString()
	{
		return "\"" + this.val + "\"";
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtStrValFormula))
		{
			return false;
		}
		return super.equals(this)  // Does canEqual
				&& this.val == ((AssrtStrValFormula) o).val;
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtStrValFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 6911;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.val.hashCode();
		return hash;
	}
}
