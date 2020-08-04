package org.scribble.ext.assrt.core.type.formula;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.sosy_lab.java_smt.api.BooleanFormula;

public class AssrtForallFormula extends AssrtQuantifiedFormula
{
	// Pre: vars non empty
	protected AssrtForallFormula(List<AssrtAVarFormula> vars, AssrtBFormula expr)
	{
		super(vars, expr);
	}

	@Override
	public AssrtForallFormula disamb(Map<AssrtVar, DataName> env)
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
		return (vars.isEmpty()) ? expr : AssrtFormulaFactory.AssrtForallFormula(vars, expr);
	}

	@Override
	public AssrtForallFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		if (this.vars.contains(old))
		{
			return this;
		}
		return AssrtFormulaFactory.AssrtForallFormula(this.vars,
				this.expr.subs(old, neu));
	}
	
	@Override
	public String toSmt2Formula(Map<AssrtVar, DataName> env)
	{
		String vs = this.vars.stream().map(v -> getSmt2VarDecl(env, v))
				.collect(Collectors.joining(" "));
		String expr = this.expr.toSmt2Formula(env);
		return "(forall (" + vs + ") " + expr + ")";
	}

	protected static String getSmt2VarDecl(Map<AssrtVar, DataName> env,
			AssrtAVarFormula v)
	{
		if (v instanceof AssrtVarFormula)
		{
			String name = v.toString();
			AssrtVar tmp = new AssrtVar(name);
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
					AssrtVar hack = new AssrtVar(name.substring(1));
					if (env.containsKey(hack))
					{
						sort = env.get(hack);
					}
					else if (name.contains("__"))  // FIXME HACK cf. AssrtCoreSConfig::makeFreshIntVar 
					{
						hack = new AssrtVar(
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
		/*QuantifiedFormulaManager qfm = JavaSmtWrapper.getInstance().qfm;
		BooleanFormula expr = this.expr.toJavaSmtFormula();
		List<IntegerFormula> vs = this.vars.stream().map(v -> v.getJavaSmtFormula()).collect(Collectors.toList());
		return qfm.forall(vs, expr);*/
		throw new RuntimeException("Deprecated");
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
		if (!(o instanceof AssrtForallFormula))
		{
			return false;
		}
		return super.equals(this);  // Does canEqual
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtForallFormula;
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
