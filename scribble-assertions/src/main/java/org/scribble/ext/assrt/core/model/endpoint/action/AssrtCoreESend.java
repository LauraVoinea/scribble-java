package org.scribble.ext.assrt.core.model.endpoint.action;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.model.endpoint.AssrtCoreEModelFactory;
import org.scribble.ext.assrt.core.model.global.AssrtCoreSModelFactory;
import org.scribble.ext.assrt.core.model.global.action.AssrtCoreSSend;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.model.endpoint.action.AssrtESend;

public class AssrtCoreESend extends AssrtESend implements AssrtCoreEAction
{
	// Annot needed -- e.g. mu X(x:=..) . mu Y(y:=..) ... X<123> -- rec var X will be discarded, so edge action needs to record which var is being updated -- no: now relying on surface syntax to only allow subprotos with proper var scoping and annotvar arity checks, etc.
	/*public final AssrtDataTypeVar annot;  // Not null (by AssrtCoreGProtocolTranslator)
	public final AssrtArithFormula expr;*/
	public final List<AssrtAFormula> sexprs;  // State exprs

	//public final LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom;
	public final List<AssrtAnnotDataName> phantom;
	public final AssrtBFormula phantAss;

	public AssrtCoreESend(ModelFactory mf, Role peer, MsgId<?> mid,
			Payload payload, AssrtBFormula ass, List<AssrtAFormula> stateexprs,
			//LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom,
			List<AssrtAnnotDataName> phantom,
			AssrtBFormula phantAss)
	{
		super(mf, peer, mid, payload, ass);
		//this.annot = annot;
		this.sexprs = Collections.unmodifiableList(stateexprs);
		//this.phantom = new LinkedHashMap<>(phantom);
		this.phantom = new LinkedList<>(phantom);
		this.phantAss = phantAss;
	}
	
	@Deprecated
	public ModelFactory getModelFactory()
	{
		return this.mf;
	}
	
	// Used by AssrtCoreSSingleBuffers.canReceive and AssrtCoreSConfig.getStuckMessages
	@Override
	public AssrtCoreESend toTrueAssertion()  // CHECKME: for model building, currently need send assertion to match (syntactical equal) receive assertion -- which is always True(?), to be fireable
	{
		return ((AssrtCoreEModelFactory) this.mf.local).AssrtCoreESend(this.peer,
				this.mid, this.payload, AssrtTrueFormula.TRUE, this.sexprs,
				this.phantom, AssrtTrueFormula.TRUE);  // CHECKME
	}

	@Override
	public AssrtCoreERecv toDual(Role self)
	{
		return ((AssrtCoreEModelFactory) this.mf.local).AssrtCoreERecv(self,
				this.mid, this.payload, this.ass, this.sexprs,
				this.phantom, this.phantAss);
	}

	@Override
	public AssrtCoreSSend toGlobal(Role self)
	{
		return ((AssrtCoreSModelFactory) this.mf.global).AssrtCoreSSend(self,
				this.peer, this.mid, this.payload, this.ass, this.sexprs);
	}

	/*@Override
	public AssrtDataTypeVar getAnnotVar()
	{
		return this.annot;
	}*/

	@Override
	//public AssrtArithFormula getArithExpr()
	public List<AssrtAFormula> getStateExprs()
	{
		return this.sexprs;
	}

	@Override
	//public LinkedHashMap<AssrtIntVar, AssrtAFormula> getPhantoms()
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
				+ phantomsToString()
				+ phantomAssertionToString()  // cf. super.assertionToString
				+ stateExprsToString();  // "First", assertion must hold; "second" pass sexprs
	}
	
	@Override
	public int hashCode()
	{
		int hash = 6779;
		hash = 31 * hash + super.hashCode();
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
		if (!(o instanceof AssrtCoreESend))
		{
			return false;
		}
		AssrtCoreESend as = (AssrtCoreESend) o;
		return super.equals(o)  // Does canEquals
				//&& this.annot.equals(as.annot) 
				&& this.sexprs.equals(as.sexprs)
				&& this.phantom.equals(as.phantom)
				&& this.phantAss.equals(as.phantAss);
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtCoreESend;
	}
}
