package org.scribble.ext.assrt.core.model.endpoint.action;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.model.endpoint.AssrtEModelFactory;
import org.scribble.ext.assrt.core.model.global.AssrtSModelFactory;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;

public class AssrtESend extends ESend implements AssrtEAction
{
	//public final AssrtAssertion assertion;  // Cf., e.g., ALSend
	public final AssrtBFormula ass;  // Not null -- empty set to True by parsing

	// Annot needed -- e.g. mu X(x:=..) . mu Y(y:=..) ... X<123> -- rec var X will be discarded, so edge action needs to record which var is being updated -- no: now relying on surface syntax to only allow subprotos with proper var scoping and annotvar arity checks, etc.
	/*public final AssrtDataTypeVar annot;  // Not null (by AssrtCoreGProtocolTranslator)
	public final AssrtArithFormula expr;*/
	public final List<AssrtAFormula> sexprs;  // State exprs

	//public final LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom;
	public final List<AssrtAnnotDataName> phantom;
	public final AssrtBFormula phantAss;

	public AssrtESend(ModelFactory mf, Role peer, MsgId<?> mid,
			Payload payload, AssrtBFormula ass, List<AssrtAFormula> stateexprs,
			List<AssrtAnnotDataName> phantom,
			AssrtBFormula phantAss)
	{
		super(mf, peer, mid, payload);
		this.ass = ass;
		//this.annot = annot;
		this.sexprs = Collections.unmodifiableList(stateexprs);
		this.phantom = new LinkedList<>(phantom);
		this.phantAss = phantAss;
	}
	
	@Override
	public AssrtBFormula getAssertion()
	{
		return this.ass;
	}

	// Used by AssrtCoreSSingleBuffers.canReceive and AssrtCoreSConfig.getStuckMessages
	public AssrtESend toTrueAssertion()  // CHECKME: for model building, currently need send assertion to match (syntactical equal) receive assertion -- which is always True(?), to be fireable
	{
		return ((AssrtEModelFactory) this.mf.local).AssrtESend(this.peer,
				this.mid, this.payload, AssrtTrueFormula.TRUE, this.sexprs,
				this.phantom, AssrtTrueFormula.TRUE);
	}

	@Override
	public AssrtERecv toDual(Role self)
	{
		return ((AssrtEModelFactory) this.mf.local).AssrtERecv(self,
				this.mid, this.payload, this.ass, this.sexprs,
				this.phantom, this.phantAss);
	}

	@Override
	public AssrtSSend toGlobal(Role self)
	{
		return ((AssrtSModelFactory) this.mf.global).AssrtCoreSSend(self,
				this.peer, this.mid, this.payload, this.ass, this.sexprs);
	}

	/*@Override
	public AssrtDataTypeVar getAnnotVar()
	{
		return this.annot;
	}*/

	@Override
	public List<AssrtAFormula> getStateExprs()
	{
		return this.sexprs;
	}

	@Override
	public List<AssrtAnnotDataName> getPhantoms()
	{
		return this.phantom;
	}

	@Override
	public AssrtBFormula getPhantomAssertion()
	{
		return this.phantAss;
	}
	
	@Override
	public String toString()
	{
		return super.toString()
				+ assertionToString()
				+ phantomsToString()
				+ phantomAssertionToString()  // cf. super.assertionToString
				+ stateExprsToString();  // "First", assertion must hold; "second" pass sexprs
	}
	
	@Override
	public int hashCode()
	{
		int hash = 6779;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.ass.toString().hashCode();  // TODO: treating as String, fix
		//hash = 31 * hash + this.annot.hashCode();
		hash = 31 * hash + this.sexprs.hashCode();
		hash = 31 * hash + this.phantom.hashCode();
		hash = 31 * hash + this.phantAss.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtESend))
		{
			return false;
		}
		AssrtESend as = (AssrtESend) o;
		return super.equals(o)  // Does canEquals
				//&& this.annot.equals(as.annot) 
				&& this.ass.toString().equals(as.ass.toString())  // TODO: treating as String, fix
				&& this.sexprs.equals(as.sexprs)
				&& this.phantom.equals(as.phantom)
				&& this.phantAss.equals(as.phantAss);
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtESend;
	}
}
