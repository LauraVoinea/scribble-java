package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

import java.util.*;
import java.util.stream.Collectors;

public class AssrtFormalGRecVar extends AssrtFormalTypeBase
		implements AssrtFormalGType
{
	public final RecVar recvar;

	public final Map<AssrtVar, AssrtAFormula> statevars;
	public final AssrtBFormula assertion;  // consolidated refinement

	protected AssrtFormalGRecVar(RecVar recvar, LinkedHashMap<AssrtVar, AssrtAFormula> svars, AssrtBFormula ass)
	{
		this.recvar = recvar;
		this.statevars = Collections.unmodifiableMap(new LinkedHashMap<>(svars));
		this.assertion = ass;
	}

	@Override
	public AssrtFormalLType project(AssrtFormalLFactory lf, Role r, AssrtPhi phi) {
		throw new RuntimeException("TODO");
	}

	@Override
	public String toString() {
		return this.recvar + "<"
				+ this.statevars.entrySet().stream()
						.map(x -> {
							AssrtAFormula p = x.getValue();
							return x.getKey() + " := " + p;
						}).collect(Collectors.joining(", "))
				+ ">";
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalGRecVar))
		{
			return false;
		}
		AssrtFormalGRecVar them = (AssrtFormalGRecVar) o;
		return super.equals(o)  // Checks canEquals -- implicitly checks kind
				&& this.recvar.equals(them.recvar)
				&& this.statevars.equals(them.statevars)
				&& this.assertion.equals(them.assertion);
	}
	
	@Override
	public boolean canEquals(Object o) {
		return o instanceof AssrtFormalGRecVar;
	}
	
	@Override
	public int hashCode()
	{
		int hash = AssrtFormalGType.RECVAR_HASH;
		hash = 31 * hash + this.recvar.hashCode();
		hash = 31 * hash + this.statevars.hashCode();
		hash = 31 * hash + this.assertion.hashCode();
		return hash;
	}
}
