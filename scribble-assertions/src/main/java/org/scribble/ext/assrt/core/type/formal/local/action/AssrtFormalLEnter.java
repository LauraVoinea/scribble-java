package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

public class AssrtFormalLEnter implements AssrtFormalLAction
{
	public final List<AssrtMsg> silents;  // TODO: factor out a "derived action" interface

	public final RecVar recvar;
	public final AssrtVar svar;
	public final Multiplicity multip;
	public final DataName data;  // AssrtAnnotDataName is the pair var, data
	public final AssrtAFormula init; // init expr -- null if silent
	public final AssrtBFormula assertion;  // (consolidated) refinement -- but currently hardcoded to single var

	public AssrtFormalLEnter(RecVar recvar, AssrtVar svar, Multiplicity multip,
							 DataName data, AssrtAFormula init, AssrtBFormula assertion,
							 List<AssrtMsg> silents) {
		this.recvar = recvar;
		this.svar = svar;
		this.multip = multip;
		this.data = data;
		this.init = init;
		this.assertion = assertion;
		this.silents = silents.stream().collect(Collectors.toList());
	}

	public AssrtFormalLEnter prepend(AssrtMsg m) {
		List<AssrtMsg> ms = new LinkedList<>(this.silents);
		ms.add(0, m);
		return AssrtFormalLFactory.factory.enter(this.recvar, this.svar, this.multip, this.data, this.init, this.assertion, ms);
	}

	public AssrtFormalLEnter drop() {
		return AssrtFormalLFactory.factory.enter(this.recvar, this.svar, this.multip, this.data, this.init, this.assertion);
	}

	@Override
	public String toString() {
		return (this.silents.isEmpty() ? "" : this.silents) +
				" " + this.recvar + "(" + this.svar +
				"^" + this.multip + ":" + this.data + "{" + this.assertion + "}" +
				(this.init == null ? "" : " := " + this.init) + ")";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.ENTER_HASH;
		hash = 31 * hash + this.silents.hashCode();
		hash = 31 * hash + this.recvar.hashCode();
		hash = 31 * hash + this.svar.hashCode();
		hash = 31 * hash + this.multip.hashCode();
		hash = 31 * hash + this.data.hashCode();
		hash = 31 * hash + this.init.hashCode();
		hash = 31 * hash + this.assertion.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalLEnter))
		{
			return false;
		}
		AssrtFormalLEnter them = (AssrtFormalLEnter) o;
		return //them.canEquals(this) &&
			this.silents.equals(them.silents) && this.recvar.equals(them.recvar)
					&& this.svar.equals(them.svar) && this.multip == them.multip
					&& this.data.equals(them.data) && this.init.equals(them.init)
					&& this.assertion.equals(them.assertion);
	}

	//public abstract boolean canEquals(Object o);
}
