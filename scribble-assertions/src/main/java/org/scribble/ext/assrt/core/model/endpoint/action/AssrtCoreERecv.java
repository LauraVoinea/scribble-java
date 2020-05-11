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
import org.scribble.ext.assrt.core.model.global.action.AssrtCoreSRecv;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.model.endpoint.action.AssrtERecv;

public class AssrtCoreERecv extends AssrtERecv implements AssrtCoreEAction
{
	// Annot needed -- e.g. mu X(x:=..) . mu Y(y:=..) ... X<123> -- rec var X will be discarded, so edge action needs to record which var is being updated
	/*public final AssrtDataTypeVar annot;  // Not null (by AssrtCoreGProtocolTranslator)
	public final AssrtArithFormula expr;*/
	public final List<AssrtAFormula> stateexprs;  // N.B. state args ignored for recv fireable and firing (msgs don't carry state args)

	//public final LinkedHashMap<AssrtIntVar, AssrtAFormula> phantom;
	public final List<AssrtAnnotDataName> phantom;  // TODO: sorts
	public final AssrtBFormula phantAss;

	public AssrtCoreERecv(ModelFactory mf, Role peer, MsgId<?> mid,
			Payload payload, AssrtBFormula bf, List<AssrtAFormula> stateexprs,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		super(mf, peer, mid, payload, bf);
		//this.annot = annot;list)
	
		this.stateexprs = Collections
				.unmodifiableList(new LinkedList<>(stateexprs));
		this.phantom = Collections.unmodifiableList(new LinkedList<>(phantom));
		this.phantAss = phantAss;
	}
	
	// Used by AssrtCoreSSingleBuffers.canReceive -- msg does not carry state args -- recv getFireable and fire follows accordingly
	// Also: AssrtSConfig.async -> AssrtCoreESend.toTrueAssertion
	public AssrtCoreERecv dropStateArgs()
	{
		return ((AssrtCoreEModelFactory) this.mf.local).AssrtCoreERecv(this.peer, this.mid,
				this.payload, this.ass, Collections.emptyList(),
				new LinkedList<>(), AssrtTrueFormula.TRUE);  // CHECKME
	}
	
	@Override
	public AssrtCoreESend toDual(Role self)
	{
		return ((AssrtCoreEModelFactory) this.mf.local).AssrtCoreESend(self, this.mid,
				this.payload, this.ass, this.stateexprs,
				this.phantom, this.phantAss);
	}

	@Override
	public AssrtCoreSRecv toGlobal(Role self)
	{
		return ((AssrtCoreSModelFactory) this.mf.global).AssrtCoreSRecv(self,
				this.peer, this.mid, this.payload, this.ass, this.stateexprs);  // FIXME: phantoms
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
		//return this.expr;
		return this.stateexprs;
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
		//return super.toString() + "@" + this.ass + ";";
		return super.toString()
				+ phantomsToString()
				+ phantomAssertionToString()  // cf. super.assertionToString
				+ stateExprsToString();  // "First", assertion must hold; "second" pass sexprs
				//+ ((this.annot.toString().startsWith("_dum")) ? "" : "<" + this.annot + " := " + this.expr + ">");  // FIXME
				//+ (this.stateexprs.isEmpty() ? "" : "<" + this.stateexprs.stream().map(Object::toString).collect(Collectors.joining(", ")) + ">");
		/*return this.obj + getCommSymbol() + this.mid + this.payload
				+ stateExprsToString() + assertionToString();*/
	}
	
	@Override
	public int hashCode()
	{
		int hash = 6763;
		hash = 31 * hash + super.hashCode();
		//hash = 31 * hash + this.annot.hashCode();
		hash = 31 * hash + this.stateexprs.hashCode();
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
		if (!(o instanceof AssrtCoreERecv))
		{
			return false;
		}
		AssrtCoreERecv as = (AssrtCoreERecv) o;
		return super.equals(o)  // Does canEquals
				//&& this.annot.equals(as.annot)
				&& this.stateexprs.equals(as.stateexprs)
				&& this.phantom.equals(as.phantom)
				&& this.phantAss.equals(as.phantAss);
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtCoreERecv;
	}
}
