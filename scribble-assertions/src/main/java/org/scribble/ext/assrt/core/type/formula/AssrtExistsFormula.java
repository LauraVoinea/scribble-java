package org.scribble.ext.assrt.core.type.formula;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

// FIXME: not only int vars now -- cf. AssrtIntVar renaming
public class AssrtExistsFormula extends AssrtQuantifiedFormula
{
	// Pre: vars non empty
	protected AssrtExistsFormula(List<AssrtAVarFormula> vars, AssrtBFormula expr)
	{
		super(vars, expr);
	}

	@Override
	public AssrtExistsFormula disamb(Map<AssrtVar, DataName> env)  // IntVar now stands for all var types
	{
		throw new RuntimeException("Won't get in here: " + this);  // Not a parsed syntax
	}
	
	@Override
	public AssrtBFormula squash()
	{
		List<AssrtAVarFormula> vars = this.vars.stream()
				.filter(v -> !v.toString().startsWith("_dum"))
				.collect(Collectors.toList());  // FIXME
		AssrtBFormula expr = this.expr.squash();
		return vars.isEmpty() ? expr
				: AssrtFormulaFactory.AssrtExistsFormula(vars, expr);
	}

	@Override
	public AssrtExistsFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		if (this.vars.contains(old))
		{
			return this;
		}
		return AssrtFormulaFactory.AssrtExistsFormula(this.vars,
				this.expr.subs(old, neu));
	}
	
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		String vs = this.vars.stream()
				.map(v -> AssrtForallFormula.getSmt2VarDecl(env, v))
				.collect(Collectors.joining(" "));
		String expr = this.expr.toSmt2Formula(env);
		return "(exists (" + vs + ") " + expr + ")";
	}
	
	@Override
	public String toString()
	{
		return "(exists " + bodyToString() + ")";
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtExistsFormula))
		{
			return false;
		}
		return super.equals(this);  // Does canEqual
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtExistsFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 6367;
		hash = 31 * hash + super.hashCode();
		return hash;
	}
}
