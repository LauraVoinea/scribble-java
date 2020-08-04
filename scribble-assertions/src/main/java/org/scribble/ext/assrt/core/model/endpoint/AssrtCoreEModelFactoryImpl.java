package org.scribble.ext.assrt.core.model.endpoint;

import java.util.List;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreEAcc;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreERecv;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreEReq;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreESend;
import org.scribble.ext.assrt.core.model.global.AssrtCoreEMsg;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.model.endpoint.AssrtEModelFactoryImpl;

public class AssrtCoreEModelFactoryImpl extends AssrtEModelFactoryImpl
		implements AssrtCoreEModelFactory
{

	public AssrtCoreEModelFactoryImpl(ModelFactory mf)
	{
		super(mf);
	}

	@Override
	public AssrtCoreESend AssrtCoreESend(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		return new AssrtCoreESend(this.mf, peer, mid, pay, ass, sexprs, phantom,
				phantAss);
	}

	@Override
	public AssrtCoreERecv AssrtCoreERecv(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		return new AssrtCoreERecv(this.mf, peer, mid, pay, ass, sexprs, phantom,
				phantAss);
	}

	@Override
	public AssrtCoreEReq AssrtCoreEReq(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		return new AssrtCoreEReq(this.mf, peer, mid, pay, ass, sexprs);
	}

	@Override
	public AssrtCoreEAcc AssrtCoreEAcc(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		return new AssrtCoreEAcc(this.mf, peer, mid, pay, ass, sexprs);
	}
	
	@Override
	public AssrtCoreEMsg AssrtCoreEMsg(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass)//, List<AssrtAFormula> sexprs)
			//Map<AssrtIntVarFormula, AssrtIntVarFormula> shadow)
	{
		return new AssrtCoreEMsg(this.mf, peer, mid, pay, ass);//, sexprs);//, shadow);
	}
}
