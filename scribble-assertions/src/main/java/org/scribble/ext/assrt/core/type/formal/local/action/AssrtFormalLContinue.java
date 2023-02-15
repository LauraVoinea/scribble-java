package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLFactory;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class AssrtFormalLContinue implements AssrtFormalLDerivedAction
{
	public final List<AssrtMsg> silents;

	public final RecVar recvar;
	public final AssrtVar svar;
	public final Multiplicity multip;
	public final AssrtAFormula init; // init expr -- null if silent

	public AssrtFormalLContinue(RecVar recvar, AssrtVar svar, Multiplicity multip,
								AssrtAFormula init, List<AssrtMsg> silents) {
		this.recvar = recvar;
		this.svar = svar;
		this.multip = multip;
		this.init = init;
		this.silents = silents.stream().collect(Collectors.toList());
	}

	@Override
	public List<AssrtMsg> getSilent() {
		return this.silents;
	}

	@Override
	public AssrtFormalLContinue prependSilent(AssrtMsg m) {
		List<AssrtMsg> ms = new LinkedList<>(this.silents);
		ms.add(0, m);
		return AssrtFormalLFactory.factory.continu(this.recvar, this.svar, this.multip, this.init, ms);
	}

	@Override
	public AssrtFormalLContinue drop() {
		return AssrtFormalLFactory.factory.continu(this.recvar, this.svar, this.multip, this.init);
	}

	@Override
	public String toString() {
		return (this.silents.isEmpty() ? "" : this.silents) +
				" " + this.recvar + "<" + this.svar +
				"^" + this.multip + (this.init == null ? "" : " := " + this.init) + ">";
	}

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.CONTINUE_HASH;
		hash = 31 * hash + this.silents.hashCode();
		hash = 31 * hash + this.recvar.hashCode();
		hash = 31 * hash + this.svar.hashCode();
		hash = 31 * hash + this.multip.hashCode();
		hash = 31 * hash + Objects.hashCode(this.init);
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalLContinue))
		{
			return false;
		}
		AssrtFormalLContinue them = (AssrtFormalLContinue) o;
		return //them.canEquals(this) &&
			this.silents.equals(them.silents) && this.recvar.equals(them.recvar)
					&& this.svar.equals(them.svar) && this.multip == them.multip
					&& Objects.equals(this.init, them.init);
	}

	//public abstract boolean canEquals(Object o);
}
