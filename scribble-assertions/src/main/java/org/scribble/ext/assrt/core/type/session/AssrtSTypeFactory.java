package org.scribble.ext.assrt.core.type.session;

import java.util.List;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.session.STypeFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.global.AssrtGTypeFactory;
import org.scribble.ext.assrt.core.type.session.local.AssrtLTypeFactory;


public class AssrtSTypeFactory extends STypeFactory
{
	// Shadows super
	public final AssrtGTypeFactory global;
	public final AssrtLTypeFactory local;
	
	public AssrtSTypeFactory(AssrtGTypeFactory global,
			AssrtLTypeFactory local)
	{
		super(global, local);
		this.global = global;
		this.local = local;
	}
	
	// Pre: op/pay/bform not null -- phantom/phantAss null for global
	public AssrtMsg AssrtCoreAction(Op op, List<AssrtAnnotDataName> pay,
			AssrtBFormula bform)
	{
		return AssrtCoreAction(op, pay, bform, null, null);
	}

	// Locals
	public AssrtMsg AssrtCoreAction(Op op, List<AssrtAnnotDataName> pay,
			AssrtBFormula bform, List<AssrtAnnotDataName> phantom,
			AssrtBFormula phantAss)
	{
		return new AssrtMsg(op, pay, bform, phantom, phantAss);
	}
}
