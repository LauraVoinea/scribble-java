package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.AssrtFormalTypeBase;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formal.local.AssrtLambda;
import org.scribble.ext.assrt.core.type.formal.local.AssrtRho;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLAction;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtFormalLEnter;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// !!! Don't really need to separate silent and non-silent rec and recvar constructors -- and multiple state vars can be done in one step (cf. comm pay vars), the step is the rec(var), not the svars
public class AssrtFormalGRec extends AssrtFormalTypeBase
		implements AssrtFormalGType
{
	public final RecVar recvar;
	public final AssrtFormalGType body;

	public final Map<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> statevars;  // var -> (roles, pay, init exprs)
	public final AssrtBFormula assertion;  // consolidated refinement

	protected AssrtFormalGRec(RecVar recvar, AssrtFormalGType body,
                              LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> svars,
                              AssrtBFormula ass) {
		this.recvar = recvar;
		this.statevars = Collections.unmodifiableMap(new LinkedHashMap<>(svars));
		this.body = body;
		this.assertion = ass;
	}

	@Override
	public AssrtFormalLType project(AssrtFormalLFactory lf, Role r) {
		throw new RuntimeException("TODO");
	}

	@Override
	public String toString() {
		return "mu " + this.recvar + "("
				+ this.statevars.entrySet().stream()
				.map(x -> {
					Triple<Set<Role>, DataName, AssrtAFormula> p = x.getValue();
					return x.getKey() + "^" + p.left + ":" + p.middle +
							(p.right == null ? "" : " := " + p.right);
				}).collect(Collectors.joining(", "))
				+ "){" + this.assertion + "}." + this.body;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalGRec))
		{
			return false;
		}
		AssrtFormalGRec them = (AssrtFormalGRec) o;
		return super.equals(o)  // Checks canEquals -- implicitly checks kind
				&& this.recvar.equals(them.recvar)
				&& this.body.equals(them.body)
				&& this.statevars.equals(them.statevars)
				&& this.assertion.equals(them.assertion);
	}
	
	@Override
	public boolean canEquals(Object o) {
		return o instanceof AssrtFormalGRec;
	}
	
	@Override
	public int hashCode()
	{
		int hash = AssrtFormalGType.REC_HASH;
		hash = 31 * hash + this.recvar.hashCode();
		hash = 31 * hash + this.body.hashCode();
		hash = 31 * hash + this.statevars.hashCode();
		hash = 31 * hash + this.assertion.hashCode();
		return hash;
	}
}
