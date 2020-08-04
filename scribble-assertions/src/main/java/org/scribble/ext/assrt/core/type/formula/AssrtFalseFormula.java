package org.scribble.ext.assrt.core.type.formula;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.sosy_lab.java_smt.api.BooleanFormula;

public class AssrtFalseFormula extends AssrtBFormula
{
	public static final AssrtFalseFormula FALSE = new AssrtFalseFormula();
	
	private AssrtFalseFormula()
	{
		
	}

	@Override
	public AssrtFalseFormula disamb(Map<AssrtVar, DataName> env)
	{
		return this;
	}
	
	@Override
	public AssrtBFormula squash()
	{
		return this;
	}

	@Override
	public AssrtFalseFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		return this;
	}
	
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		return "false";
	}
	
	@Override
	protected BooleanFormula toJavaSmtFormula() //throws AssertionParseException {
	{
		//return JavaSmtWrapper.getInstance().bfm.makeFalse();
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
		return "False"; 
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFalseFormula))
		{
			return false;
		}
		return super.equals(this);  // Does canEqual
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtFalseFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 5881;
		hash = 31 * hash + super.hashCode();
		return hash;
	}
}
