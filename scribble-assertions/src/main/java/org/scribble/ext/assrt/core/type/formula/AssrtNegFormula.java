package org.scribble.ext.assrt.core.type.formula;

import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public class AssrtNegFormula extends AssrtBFormula
{
	public final AssrtBFormula expr;

	protected AssrtNegFormula(AssrtBFormula expr)
	{
		this.expr = expr;
	}

	@Override
	public AssrtNegFormula disamb(Map<AssrtVar, DataName> env)
	{
		return new AssrtNegFormula((AssrtBFormula) this.expr.disamb(env));  // CHECKME: use factory?
	}

	@Override
	public AssrtBFormula squash()
	{
		return AssrtFormulaFactory.AssrtNeg(this.expr.squash());
	}

	@Override
	public AssrtBFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		return AssrtFormulaFactory.AssrtNeg(this.expr.subs(old, neu));
	}

	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		return "(not " + this.expr.toSmt2Formula(env) + ")";
	}

	@Override
	public Set<AssrtVar> getIntVars()
	{
		Set<AssrtVar> vs = this.expr.getIntVars();
		return vs;
	}
	
	@Override
	public String toString()
	{
		return "!" + this.expr;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtNegFormula))
		{
			return false;
		}
		AssrtNegFormula f = (AssrtNegFormula) o;
		return super.equals(this)  // Does canEqual
				&& this.expr.equals(f.expr);  
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtNegFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 7109;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.expr.hashCode();
		return hash;
	}
}
