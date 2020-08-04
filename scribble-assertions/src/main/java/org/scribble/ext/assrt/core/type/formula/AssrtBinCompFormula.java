package org.scribble.ext.assrt.core.type.formula;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

// Binary comparison
public class AssrtBinCompFormula extends AssrtBFormula
		implements AssrtBinFormula
{
	public enum Op
	{
		Eq,
		LessThan, 
		LessThanEq, 
		GreaterThan, 
		GreaterThanEq;
		
		@Override
		public String toString()
		{
			switch (this)
			{
				case Eq: return "=";
				case LessThan: return "<";
				case LessThanEq: return "<=";
				case GreaterThan: return ">";
				case GreaterThanEq: return ">=";
				default: throw new RuntimeException("Won't get in here: " + this);
			}
		}
	}

	public final Op op; 
	public final AssrtAFormula left; 
	public final AssrtAFormula right; 
	
	protected AssrtBinCompFormula(Op op, AssrtAFormula left, AssrtAFormula right)
	{
		this.left = left; 
		this.right = right; 
		this.op = op;
		/*switch (op) {
		case ">": 
			this.op = Op.GreaterThan;
			break; 
		case "<":
			this.op = Op.LessThan;
			break;
		case "=":
			this.op = Op.Eq;
			break;
		default: throw new RuntimeException("[assrt] Shouldn't get in here: " + op);
		}*/
	}

	@Override
	public AssrtBinCompFormula disamb(Map<AssrtVar, DataName> env)
	{
		return new AssrtBinCompFormula(this.op,
				(AssrtAFormula) this.left.disamb(env),
				(AssrtAFormula) this.right.disamb(env));
	}

	@Override
	//public AssrtBinCompFormula squash()
	public AssrtBFormula squash()  // For True
	{
		if (this.op.equals(AssrtBinCompFormula.Op.Eq)
				&& (this.left instanceof AssrtVarFormula) && this.left.toString().startsWith("_dum"))  // FIXME
		{
			return AssrtTrueFormula.TRUE;
		}
		//return this;  // No: underlying this.formula will not be the same after squashing
		return AssrtFormulaFactory.AssrtBinComp(this.op, this.left.squash(), this.right.squash());
	}

	@Override
	public AssrtBinCompFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		return AssrtFormulaFactory.AssrtBinComp(this.op, this.left.subs(old, neu), this.right.subs(old, neu));
	}
	
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		String left = this.left.toSmt2Formula(env);
		String right = this.right.toSmt2Formula(env);
		String op;
		switch(this.op)
		{
			case Eq:            op = "=";  break;
			case LessThan:      op = "<";  break;
			case LessThanEq:    op = "<="; break;
			case GreaterThan:   op = ">";  break;
			case GreaterThanEq: op = ">="; break;
			default: throw new RuntimeException("[assrt] Shouldn't get in here: " + this.op);
		}
		return "(" + op + " " + left + " " + right + ")";
	}
	
	@Override
	public Set<AssrtVar> getIntVars()
	{
		Set<AssrtVar> vars = new HashSet<>(this.left.getIntVars()); 
		vars.addAll(this.right.getIntVars()); 
		return vars; 
	}

	@Override
	public AssrtAFormula getLeft()
	{
		return this.left;
	}

	@Override
	public AssrtAFormula getRight()
	{
		return this.right;
	}
	
	@Override
	public String toString()
	{
		return "(" + this.left.toString() + ' '  + this.op + ' ' + this.right.toString() + ")"; 
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtBinCompFormula))
		{
			return false;
		}
		AssrtBinCompFormula f = (AssrtBinCompFormula) o;
		return super.equals(this)  // Does canEqual
				&& this.op.equals(f.op) && this.left.equals(f.left) && this.right.equals(f.right);  
						// Storing left/right as a Set could give commutativity in equals, but not associativity
						// Better to keep "syntactic" equality, and do via additional routines for, e.g., normal forms
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtBinCompFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 5897;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.op.hashCode();
		hash = 31 * hash + this.left.hashCode();
		hash = 31 * hash + this.right.hashCode();
		return hash;
	}
}
