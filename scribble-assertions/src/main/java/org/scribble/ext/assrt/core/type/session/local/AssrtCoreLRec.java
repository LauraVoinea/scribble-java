package org.scribble.ext.assrt.core.type.session.local;

import java.util.LinkedHashMap;
import java.util.stream.Collectors;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtIntVar;
import org.scribble.ext.assrt.core.type.session.AssrtCoreRec;

public class AssrtCoreLRec extends AssrtCoreRec<Local, AssrtCoreLType>
		implements AssrtCoreLType
{
	public final LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom;

	protected AssrtCoreLRec(CommonTree source, RecVar rv, AssrtCoreLType body,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> svars, AssrtBFormula ass,
			LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom)
	{
		super(source, rv, body, svars, ass);
		this.phantom = phantom;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtCoreLRec))
		{
			return false;
		}
		return super.equals(obj)  // Does canEquals
				&& this.phantom.equals(((AssrtCoreLRec) obj).phantom);
	}
	
	@Override
	public String toString()
	{
		return "mu " + this.recvar + "<"
				+ this.statevars.entrySet().stream()
						.map(x -> x.getKey() + " := " + x.getValue()).collect(  // FIXME: sort
								Collectors.joining(", "))
				+ ">"
				+ "[" + this.phantom.entrySet().stream()
						.map(x -> x.getKey() + ":=" + x.getValue())  // FIXME: sort
						.collect(Collectors.joining(", "))
				+ "]"
				+ this.assertion
				//+ this.phantom.entrySet().stream().map(x -> "&&" + x.getValue()).collect(Collectors.joining())  // No: currently actual assertion is already monolithic (here is init expr)
				+ "." + this.body;
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtCoreLRec;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 2389;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.phantom.hashCode();
		return hash;
	}
}
