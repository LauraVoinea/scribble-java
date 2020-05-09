package org.scribble.ext.assrt.core.type.session;

import java.util.List;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.session.STypeFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.session.global.AssrtCoreGTypeFactory;
import org.scribble.ext.assrt.core.type.session.local.AssrtCoreLTypeFactory;


public class AssrtCoreSTypeFactory extends STypeFactory
{
	// Shadows super
	public final AssrtCoreGTypeFactory global;
	public final AssrtCoreLTypeFactory local;
	
	public AssrtCoreSTypeFactory(AssrtCoreGTypeFactory global,
			AssrtCoreLTypeFactory local)
	{
		super(global, local);
		this.global = global;
		this.local = local;
	}
	
	// Pre: op/pay/bform not null -- phantom/phantAss null for global
	public AssrtCoreMsg AssrtCoreAction(Op op, List<AssrtAnnotDataName> pay,
			AssrtBFormula bform)
	{
		return AssrtCoreAction(op, pay, bform, null, null);
	}

	// Locals
	public AssrtCoreMsg AssrtCoreAction(Op op, List<AssrtAnnotDataName> pay,
			AssrtBFormula bform, List<AssrtAnnotDataName> phantom,
			AssrtBFormula phantAss)
	{
		return new AssrtCoreMsg(op, pay, bform, phantom, phantAss);
	}
}
