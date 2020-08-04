package org.scribble.ext.assrt.core.type.formula;

import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.sosy_lab.java_smt.api.BooleanFormula;

// Binary boolean
// Top-level formula of assertions
public class AssrtBinBFormula extends AssrtBFormula implements AssrtBinFormula<BooleanFormula>
{
	public enum Op
	{
		And, 
		Or,
		Imply;  // Not currently parsed, only created

		@Override
		public String toString()
		{
			switch (this)
			{
				case And: return "&&";
				case Or: return "||";
				case Imply: return "=>";
				default: throw new RuntimeException("Won't get in here: " + this);
			}
		}
	}
		
	public final Op op; 
	public final AssrtBFormula left; 
	public final AssrtBFormula right; 
	//BooleanFormula formula;   // FIXME
	
	protected AssrtBinBFormula(Op op, AssrtBFormula left, AssrtBFormula right)
	{
		this.left = left; 
		this.right = right; 
		this.op = op;
		/*switch (op) {
		case "&&": 
			this.op = AssrtBinBoolOp.And; 
			break; 
		case "||":
			this.op = AssrtBinBoolOp.Or;
			break;
		default:
			throw new RuntimeException("[assrt] Shouldn't get in here: " + op);
		}*/
	}

	@Override
	public AssrtBinBFormula disamb(Map<AssrtVar, DataName> env)
	{
		return new AssrtBinBFormula(this.op, (AssrtBFormula) this.left.disamb(env),
				(AssrtBFormula) this.right.disamb(env));
	}
	
	@Override
	public AssrtBFormula squash()
	{
		AssrtBFormula left = this.left.squash();
		AssrtBFormula right = this.right.squash();
		switch (this.op)
		{
			case And:
			{
				if (left.equals(AssrtFalseFormula.FALSE) || right.equals(AssrtFalseFormula.FALSE))
				{
					return AssrtFalseFormula.FALSE;
				}
				if (left.equals(AssrtTrueFormula.TRUE))
				{
					return right;
				}
				if (right.equals(AssrtTrueFormula.TRUE))
				{
					return left;
				}
				return AssrtFormulaFactory.AssrtBinBool(this.op, left, right);
			}
			case Imply:
			{
				if (left.equals(AssrtFalseFormula.FALSE))
				{	
					return AssrtTrueFormula.TRUE;
				}
				/*if (right.equals(AssrtFalseFormula.FALSE))
				{
					return ..neg.. left;
				}*/
				if (right.equals(AssrtTrueFormula.TRUE))
				{
					return right;
				}
				return AssrtFormulaFactory.AssrtBinBool(this.op, left, right);
			}
			case Or:
			{
				if (left.equals(AssrtTrueFormula.TRUE) || right.equals(AssrtTrueFormula.TRUE))
				{
					return AssrtTrueFormula.TRUE;
				}
				if (left.equals(AssrtFalseFormula.FALSE))
				{
					return right;
				}
				if (right.equals(AssrtFalseFormula.FALSE))
				{
					return left;
				}
				return AssrtFormulaFactory.AssrtBinBool(this.op, left, right);
			}
			default: throw new RuntimeException("[assrt] Shouldn't get in here: " + op);
		}
	}

	@Override
	public AssrtBinBFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		return AssrtFormulaFactory.AssrtBinBool(this.op, this.left.subs(old, neu), this.right.subs(old, neu));
	}
	
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		String left = this.left.toSmt2Formula(env);
		String right = this.right.toSmt2Formula(env);
		String op;
		switch(this.op)
		{
			case And:   op = "and"; break;
			case Or:    op = "or"; break;
			case Imply: op = "=>"; break;
			default:   throw new RuntimeException("[assrt] Shouldn't get in here: " + this.op);
		}
		return "(" + op + " " + left + " " + right + ")";
	}
	
	@Override
	protected BooleanFormula toJavaSmtFormula() //throws AssertionParseException
	{
		/*BooleanFormulaManager fmanager = JavaSmtWrapper.getInstance().bfm;
		BooleanFormula bleft = this.left.toJavaSmtFormula();
		BooleanFormula bright = this.right.toJavaSmtFormula();
		switch(this.op)
		{
			case And:   return fmanager.and(bleft, bright); 
			case Or:    return fmanager.or(bleft, bright); 
			case Imply: return fmanager.implication(bleft, bright); 
			default:
				//throw new AssertionParseException("No matchin ooperation for boolean formula"); 
				throw new RuntimeException("[assrt] Shouldn't get in here: " + op);
		}*/
		throw new RuntimeException("Deprecated");
	}
	
	@Override
	public Set<AssrtVar> getIntVars()
	{
		Set<AssrtVar> vars = new HashSet<>(this.left.getIntVars()); 
		vars.addAll(this.right.getIntVars()); 
		return vars; 
	}

	@Override
	public AssrtBFormula getLeft()
	{
		return this.left;
	}

	@Override
	public AssrtBFormula getRight()
	{
		return this.right;
	}

	@Override
	public String toString()
	{
		return "(" + this.left + ' '  + this.op + ' ' + this.right + ")";  
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtBinBFormula))
		{
			return false;
		}
		AssrtBinBFormula f = (AssrtBinBFormula) o;
		return super.equals(this)  // Does canEqual
				&& this.op.equals(f.op) && this.left.equals(f.left) && this.right.equals(f.right);  
						// Storing left/right as a Set could give commutativity in equals, but not associativity
						// Better to keep "syntactic" equality, and do via additional routines for, e.g., normal forms
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtBinBFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 5881;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.op.hashCode();
		hash = 31 * hash + this.left.hashCode();
		hash = 31 * hash + this.right.hashCode();
		return hash;
	}

	public static List<AssrtBFormula> getCnfClauses(AssrtBFormula f)
	{
		List<AssrtBFormula> fs = new LinkedList<>();
		while (f instanceof AssrtBinBFormula)
		{
			AssrtBinBFormula c = (AssrtBinBFormula) f;
			if (c.op != Op.And)
			{
				break;
			}
			fs.add(c.left);
			f = c.right;
		}
		fs.add(f);
		return fs;
	}
}
