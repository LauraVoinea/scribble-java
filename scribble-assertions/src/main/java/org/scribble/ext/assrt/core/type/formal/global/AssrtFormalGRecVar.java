package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.Quadple;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class AssrtFormalGRecVar extends AssrtFormalTypeBase
		implements AssrtFormalGType
{
	public final RecVar recvar;

	public final Map<AssrtVar, AssrtAFormula> statevars;  // null value if no init

	protected AssrtFormalGRecVar(RecVar recvar, LinkedHashMap<AssrtVar, AssrtAFormula> svars)
	{
		this.recvar = recvar;
		this.statevars = Collections.unmodifiableMap(new LinkedHashMap<>(svars));
	}

	@Override
	public AssrtFormalLType project(AssrtFormalLFactory lf, Role r, AssrtPhi phi) {
		if (!phi.map.containsKey(this.recvar)) {  // TODO make Optional
			throw new RuntimeException("Shouldn't get here: " + this + " ,, " + phi.map);
		}
		LinkedHashMap<AssrtVar, Pair<Multiplicity, AssrtAFormula>> svars = new LinkedHashMap<>();

		Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> qs = phi.map.get(this.recvar);

		for (Map.Entry<AssrtVar, AssrtAFormula> e : this.statevars.entrySet()) {
			AssrtVar v = e.getKey();
			if (qs.get(v).left.contains(r)) {
				svars.put(v, new Pair<>(Multiplicity.OMEGA, e.getValue()));
			} else {
				svars.put(v, new Pair<>(Multiplicity.ZERO, null));  // !!! just omit?
			}
		}

		return lf.recvar(this.recvar, svars);
	}

	@Override
	public Set<Role> getRoles() {
		return Collections.emptySet();
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
				&& this.statevars.equals(them.statevars);
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
		return hash;
	}
}
