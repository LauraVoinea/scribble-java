package org.scribble.ext.assrt.core.type.formula;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtIntVar;
import org.scribble.ext.assrt.util.JavaSmtWrapper;
import org.sosy_lab.java_smt.api.BooleanFormula;
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula;
import org.sosy_lab.java_smt.api.QuantifiedFormulaManager;

public class AssrtForallIntVarsFormula extends AssrtQuantifiedIntVarsFormula
{
	// Pre: vars non empty
	//protected AssrtForallIntVarsFormula(List<AssrtIntVarFormula> vars, AssrtBFormula expr)
	protected AssrtForallIntVarsFormula(List<AssrtAVarFormula> vars,
			AssrtBFormula expr)
	{
		super(vars, expr);
	}

	@Override
	public AssrtForallIntVarsFormula disamb(Map<AssrtIntVar, DataName> env)
	{
		throw new RuntimeException("Won't get in here: " + this);  // Not a parsed syntax
	}

	@Override
	public AssrtBFormula getCnf()
	{
		throw new RuntimeException("[assrt-core] Shouldn't get in here: " + this);
	}

	@Override
	public boolean isNF(AssrtBinBFormula.Op op)
	{
		throw new RuntimeException("[assrt-core] Shouldn't get in here: " + this);
	}

	@Override
	public boolean hasOp(AssrtBinBFormula.Op op)
	{
		throw new RuntimeException("[assrt-core] Shouldn't get in here: " + this);
	}
	
	@Override
	public AssrtBFormula squash()
	{
		List<AssrtAVarFormula> vars = this.vars.stream()
				.filter(v -> !v.toString().startsWith("_dum"))
				.collect(Collectors.toList());  // FIXME
		AssrtBFormula expr = this.expr.squash();
		return (vars.isEmpty()) ? expr : AssrtFormulaFactory.AssrtForallFormula(vars, expr);
	}

	@Override
	public AssrtForallIntVarsFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		if (this.vars.contains(old))
		{
			return this;
		}
		return AssrtFormulaFactory.AssrtForallFormula(
				//this.vars.stream().map(v -> v.subs(old, neu)).collect(Collectors.toList()), 
				this.vars,
				this.expr.subs(old, neu));
	}
	
	@Override
	public String toSmt2Formula(Map<AssrtIntVar, DataName> env)
	{
		String vs = this.vars.stream().map(v -> getSmt2VarDecl(env, v))
				.collect(Collectors.joining(" "));
		String expr = this.expr.toSmt2Formula(env);
		return "(forall (" + vs + ") " + expr + ")";
	}

	protected static String getSmt2VarDecl(Map<AssrtIntVar, DataName> env,
			AssrtAVarFormula v)
	{
		if (v instanceof AssrtIntVarFormula)
		{
			String name = v.toString();
			AssrtIntVar tmp = new AssrtIntVar(name);
			DataName sort = null;
			if (env.containsKey(tmp))
			{
				sort = env.get(tmp);
			}
			else
			{
				// FIXME HACK statevar sorts
				if (name.startsWith("_")) // cf. AssrtCoreSGraphBuilderUtil::renameFormula and AssrtCoreSConfig::makeFreshIntVar 
				{
					AssrtIntVar hack = new AssrtIntVar(name.substring(1));
					if (env.containsKey(hack))
					{
						sort = env.get(hack);
					}
					else if (name.contains("__"))  // FIXME HACK cf. AssrtCoreSConfig::makeFreshIntVar 
					{
						hack = new AssrtIntVar(
								name.substring(1, name.lastIndexOf("_") - 1));
						if (env.containsKey(hack))
						{
							sort = env.get(hack);
						}
					}
				}
			}
			if (sort == null)
			{
				//sort = new DataName("int");
				throw new RuntimeException("Unknown var: " + v);
			}
			return "(" + v.toSmt2Formula(env) + " " + toSmt2Sort(sort)
					+ ")";
		}
		else
		{
			throw new RuntimeException("Unknown var type: " + v.getClass());
		}
	}

	@Override
	protected BooleanFormula toJavaSmtFormula()
	{
		QuantifiedFormulaManager qfm = JavaSmtWrapper.getInstance().qfm;
		BooleanFormula expr = this.expr.toJavaSmtFormula();
		List<IntegerFormula> vs = this.vars.stream().map(v -> v.getJavaSmtFormula()).collect(Collectors.toList());
		return qfm.forall(vs, expr);
	}
	
	@Override
	public String toString()
	{
		return "(forall " + bodyToString() + ")";
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtForallIntVarsFormula))
		{
			return false;
		}
		return super.equals(this);  // Does canEqual
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtForallIntVarsFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 6803;
		hash = 31 * hash + super.hashCode();
		return hash;
	}

	/* Static helpers */

	// Cf. AssrtCoreGTypeTranslator.parsePayload, AssrtCoreSConfg.getAssVars, AssrtAmbigVarFormula.disamb
	protected static String toSmt2Sort(DataName data)
	{
		switch (data.toString())
		{
		case "int":
			return "Int";
		case "string":  // TODO factor out
		case "String":
			return "String";
		default:
			throw new RuntimeException("Unsupported sort: " + data);
		}
	}
}
