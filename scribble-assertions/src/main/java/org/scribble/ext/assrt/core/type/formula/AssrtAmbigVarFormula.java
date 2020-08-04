package org.scribble.ext.assrt.core.type.formula;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.util.RuntimeScribException;

// TODO deprecate -- All vars should now be AssrtIntVar (and rename from Int)
public class AssrtAmbigVarFormula extends AssrtAVarFormula
{
	protected AssrtAmbigVarFormula(String name)
	{
		super(name);
	}

	@Override
	public AssrtSmtFormula disamb(Map<AssrtVar, DataName> env)
	{
		Optional<Entry<AssrtVar, DataName>> findAny = env.entrySet().stream()
				.filter(x -> x.getKey().toString().equals(this.name)).findAny();
		if (!findAny.isPresent())
		{
			throw new RuntimeScribException("Unknown variable: " + this.name);
		}
		Entry<AssrtVar, DataName> e = findAny.get();
		String type = e.getValue().toString();
		String name = e.getKey().toString();
		switch (type)  // HACK
		{
		case "int":
		case "String":
		case "string": // Cf. AssrtCoreGTypeTranslator.parsePayload, AssrtCoreSConfg.getAssVars, AssrtForallFormula.toSmt2Sort
			return new AssrtVarFormula(name);
		//return new AssrtStrVarFormula(name);
		default:
			throw new RuntimeScribException("Unsupported payload/state var type: "
					+ type);
		}
	}
	
	// i.e., to "type"
	@Override
	public //AssrtPayElemType<?> 
	AssrtVar toName()
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
	public DataName getSort(Map<AssrtVar, DataName> env)
	{
		//throw new RuntimeException("Shouldn't get in here: " + this.name);
		return env.get(new AssrtVar(toString()));
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
