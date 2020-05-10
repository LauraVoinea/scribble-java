package org.scribble.ext.assrt.core.type.formula;

import java.util.Map;
import java.util.Map.Entry;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtIntVar;
import org.sosy_lab.java_smt.api.NumeralFormula.IntegerFormula;

// TODO deprecate -- All vars should now be AssrtIntVar (and rename from Int)
public class AssrtAmbigVarFormula extends AssrtAVarFormula
{
	protected AssrtAmbigVarFormula(String name)
	{
		super(name);
	}

	@Override
	public AssrtSmtFormula<IntegerFormula> disamb(Map<AssrtIntVar, DataName> env)
	{
		Entry<AssrtIntVar, DataName> e = env.entrySet().stream()
				.filter(x -> x.getKey().toString().equals(this.name)).findAny().get();
		String type = e.getValue().toString();
		String name = e.getKey().toString();
		switch (type)  // HACK
		{
		case "int":
		case "String":
		case "string": // Cf. AssrtCoreGTypeTranslator.parsePayload, AssrtCoreSConfg.getAssVars, AssrtForallFormula.toSmt2Sort
			return new AssrtIntVarFormula(name);
		//return new AssrtStrVarFormula(name);
		default:
			throw new RuntimeException("Unsupported payload/state var type: " +
					type);
		}
	}
	
	// i.e., to "type"
	@Override
	public //AssrtPayElemType<?> 
	AssrtIntVar toName()
	{
		throw new RuntimeException("Shouldn't get in here: " + this.name);
	}

	@Override
	public AssrtAmbigVarFormula squash()
	{
		//return AssrtFormulaFactory.AssrtIntVar(this.name);
		throw new RuntimeException("Shouldn't get in here: " + this.name);
	}

	@Override
	public AssrtAmbigVarFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu)
	{
		throw new RuntimeException("Shouldn't get in here: " + this.name);
	}

	@Override
	public DataName getSort(Map<AssrtIntVar, DataName> env)
	{
		//throw new RuntimeException("Shouldn't get in here: " + this.name);
		return env.get(new AssrtIntVar(toString()));
	}
	
	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtAmbigVarFormula))
		{
			return false;
		}
		return super.equals(this)  // Does canEqual
				&& this.name.equals(((AssrtAmbigVarFormula) o).name);
	}
	
	@Override
	protected boolean canEqual(Object o)
	{
		return o instanceof AssrtAmbigVarFormula;
	}

	@Override
	public int hashCode()
	{
		int hash = 9463;
		hash = 31 * hash + super.hashCode();
		return hash;
	}
}
