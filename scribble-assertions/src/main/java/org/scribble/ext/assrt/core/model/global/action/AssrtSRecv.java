package org.scribble.ext.assrt.core.model.global.action;

import java.util.List;

import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;

public class AssrtSRecv extends SRecv implements AssrtSAction
{
	//public final AssrtAssertion assertion;  // Cf., e.g., AGMsgTransfer
	public final AssrtBFormula ass;  // Cf., e.g., AGMsgTransfer  // Not null (cf. AssrtEReceive)

	// Annot needed -- e.g. mu X(x:=..) . mu Y(y:=..) ... X<123> -- rec var X will be discarded, so edge action needs to record which var is being updated
	/*public final AssrtDataTypeVar annot;  // Not null (by AssrtCoreGProtocolTranslator)
	public final AssrtArithFormula expr;*/

	public final List<AssrtAFormula> stateexprs;

	public AssrtSRecv(Role subj, Role obj, MsgId<?> mid, Payload payload,
			AssrtBFormula ass, List<AssrtAFormula> stateexprs)
	{
		super(subj, obj, mid, payload);
		this.ass = ass;
		//this.annot = annot;
		this.stateexprs = stateexprs;
	}

	@Override
	public AssrtBFormula getAssertion()
	{
		return this.ass;
	}

	@Override
	public List<AssrtAFormula> getStateExprs()
	{
		return this.stateexprs;
	}
	
	@Override
	public String toString()
	{
		return super.toString()
				+ assertionToString()
				+ stateExprsToString();  // "First", assertion must hold; "second" pass sexprs
	}

	@Override
	public int hashCode()
	{
		int hash = 6791;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.ass.toString().hashCode();  // TODO: treating as String (cf. AssrtEReceive), fix
		//hash = 31 * hash + this.annot.hashCode();
		hash = 31 * hash + this.stateexprs.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtSRecv))
		{
			return false;
		}
		AssrtSRecv as = (AssrtSRecv) o;
		return super.equals(o)  // Does canEqual
				&& this.ass.toString().equals(as.ass.toString())  // TODO: treating as String (cf. AssrtEReceive), fix
				//&& this.annot.equals(as.annot)
				&& this.stateexprs.equals(as.stateexprs);
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtSRecv;
	}
}
