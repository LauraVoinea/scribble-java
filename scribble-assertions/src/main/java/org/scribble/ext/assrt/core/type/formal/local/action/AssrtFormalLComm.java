package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLType;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public abstract class AssrtFormalLComm implements AssrtFormalLDerivedAction
{
	public final List<AssrtMsg> silent;  // squashed phantoms, TODO: factor out a "derived action" interface

	public final Role peer;
	public final AssrtMsg msg;  // Phantoms here not used in formal (cf. AssrtFormalLEpsilon and  `silent` (consumed) above)

	public final Map<AssrtVar, AssrtAFormula> updates;  // unmodifiable(LinkedHashMap)

	public AssrtFormalLComm(Role peer, AssrtMsg msg, List<AssrtMsg> silent)
	{
		this(peer, msg, silent, new LinkedHashMap<>());
	}

	public AssrtFormalLComm(Role peer, AssrtMsg msg, List<AssrtMsg> silent,
							LinkedHashMap<AssrtVar, AssrtAFormula> updates)
	{
		this.peer = peer;
		this.msg = msg;
		this.silent = silent.stream().collect(Collectors.toList());
		this.updates = Collections.unmodifiableMap(new LinkedHashMap<>(updates));
	}

	public abstract AssrtFormalLComm drop();
	public abstract AssrtFormalLComm addStateUpdate(AssrtVar v, AssrtAFormula e);

	@Override
	public List<AssrtMsg> getSilent() {
		return this.silent;
	}

	@Override
	public String toString()
	{
		return (this.silent.isEmpty() ? "" : this.silent)
				+ " " + this.peer + getCommSymbol() + this.msg + updatesToString();
	}

	protected String updatesToString() {
		if (this.updates.isEmpty()) {
			return "";
		}
		String s = this.updates.toString();
		return "<" + s.substring(1, s.length()-1) + ">";
	}

	public abstract String getCommSymbol();

	@Override
	public int hashCode()
	{
		int hash = AssrtFormalLType.COMM_HASH;
		hash = 31 * hash + this.silent.hashCode();
		hash = 31 * hash + this.peer.hashCode();
		hash = 31 * hash + this.msg.hashCode();
		hash = 31 * hash + this.updates.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtFormalLComm))
		{
			return false;
		}
		AssrtFormalLComm them = (AssrtFormalLComm) o;
		return //them.canEquals(this) &&
			this.silent.equals(them.silent) && this.peer.equals(them.peer)
					&& this.msg.equals(them.msg) && this.updates.equals(them.updates);
	}

	//public abstract boolean canEquals(Object o);
}
