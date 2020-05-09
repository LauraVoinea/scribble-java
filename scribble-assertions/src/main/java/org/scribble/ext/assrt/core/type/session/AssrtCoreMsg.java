package org.scribble.ext.assrt.core.type.session;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtIntVar;


public class AssrtCoreMsg
{
	public final Op op;
	public final List<AssrtAnnotDataName> pay;
	public final AssrtBFormula ass;  // cnf?  Set?  // Not null -- empty ass set to True by AssrtCoreGProtocolDeclTranslator
	
	// null for globals
	public final List<AssrtAnnotDataName> phantom;
	public final AssrtBFormula phantAss;

	public AssrtCoreMsg(Op op, List<AssrtAnnotDataName> pay, AssrtBFormula ass,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		this.op = op;
		this.pay = Collections.unmodifiableList(new LinkedList<>(pay));
		this.ass = ass;

		this.phantom = phantom == null
				? null : Collections.unmodifiableList(new LinkedList<>(phantom));
		this.phantAss = phantAss;
	}
	
	public AssrtCoreMsg disamb(Map<AssrtIntVar, DataName> env)
	{
		return new AssrtCoreMsg(this.op, this.pay,  // CHECKME: disamb payvars unnecessary?
				(AssrtBFormula) this.ass.disamb(env),
				this.phantom,
				this.phantAss == null
						? null : (AssrtBFormula) this.phantAss.disamb(env));
	}

	@Override
	public String toString()
	{
		String pays = this.pay.toString();
		if (this.pay.size() == 1)
		{
			pays = pays.substring(1, pays.length() - 1);  // For back-compat with prev. unary pay restriction
		}
		return this.op + "(" + pays + ")" + this.ass
				+ (this.phantom == null ? ""
						: "[" + this.phantom.stream().map(x -> x + ":int")
								.collect(Collectors.joining(", ")) + "]")  // FIXME: int
				+ (this.phantAss == null ? "" : this.phantAss);
	}

	@Override
	public int hashCode()
	{
		int hash = 43;
		hash = 31 * hash + this.op.hashCode();
		hash = 31 * hash + this.pay.hashCode();
		hash = 31 * hash + this.ass.hashCode();
		if (this.phantom != null)
		{
			hash = 31 * hash + this.phantom.hashCode();
		}
		if (this.phantAss != null)
		{
			hash = 31 * hash + this.phantAss.hashCode();
		}
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (obj == this)
		{
			return true;
		}
		if (!(obj instanceof AssrtCoreMsg))
		{
			return false;
		}
		AssrtCoreMsg them = (AssrtCoreMsg) obj;
		return this.op.equals(them.op) && this.pay.equals(them.pay)
				&& this.ass.equals(them.ass)
				&& Objects.equals(this.phantom, them.phantom)
				&& Objects.equals(this.phantAss, them.phantAss);
	}
}
