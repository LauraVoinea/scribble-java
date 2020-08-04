package org.scribble.ext.assrt.core.type.formula;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.JavaSmtWrapper;
import org.sosy_lab.java_smt.api.IntegerFormulaManager;
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula;

// Integer literal
public class AssrtIntValFormula extends AssrtAFormula
{
	public static final AssrtAFormula ZERO = AssrtFormulaFactory.AssrtIntVal(0); 

	public final int val;

	protected AssrtIntValFormula(int i)
	{
		this.val = i; 
	}

	@Override
	public AssrtIntValFormula disamb(Map<AssrtVar, DataName> env)
	{
		return this;
	}

	@Override
	public AssrtIntValFormula squash()
	{
		return AssrtFormulaFactory.AssrtIntVal(this.val);
	}

	@Override
	public AssrtIntValFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		return this;
	}

	@Override
	public DataName getSort(Map<AssrtVar, DataName> env)
	{
		return new DataName("int");  // TODO: factor out constant
	}

	@Override
	public boolean isConstant()
	{
		return true;
	}
		
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		return Integer.toString(this.val);
	}
	
	@Override
	public IntegerFormula toJavaSmtFormula()
	{
		IntegerFormulaManager fmanager = JavaSmtWrapper.getInstance().ifm;
		return fmanager.makeNumber(this.val);  
	}
	
	@Override
	public Set<AssrtVar> getIntVars()
	{
		return Collections.emptySet();	
	}
	
	@Override
	public String toString()
	{
		return Integer.toString(this.val); 
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtIntValFormula))
		{
			return false;
		}
		return super.equals(this)  // Does canEqual
				&& this.val == ((AssrtIntValFormula) o).val;
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtIntValFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 5897;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.val;
		return hash;
	}
}
