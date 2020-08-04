package org.scribble.ext.assrt.core.model.endpoint;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EModelFactoryImpl;
import org.scribble.core.model.endpoint.EState;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreERecv;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreESend;
import org.scribble.ext.assrt.core.model.global.AssrtCoreEMsg;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public class AssrtCoreEModelFactoryImpl extends EModelFactoryImpl//AssrtEModelFactoryImpl
		implements AssrtCoreEModelFactory
{

	public AssrtCoreEModelFactoryImpl(ModelFactory mf)
	{
		super(mf);
	}

	@Override
	public AssrtEGraphBuilderUtil EGraphBuilderUtil()
	{
		return new AssrtEGraphBuilderUtil(this.mf);
	}

	/* Override existing types */

	@Override
	public EState EState(Set<RecVar> labs)  // Used in a more places than above "disabled" actions -- e.g., LInteractionSeqDel, to be uniform need to make an AssrtLInteractionSeqDel
	{
		return newAssrtEState(labs, new LinkedHashMap<>(),
				AssrtTrueFormula.TRUE, new LinkedHashMap<>());
	}

	/* "New" types */

	@Override
	public AssrtEState newAssrtEState(Set<RecVar> labs,
			LinkedHashMap<AssrtVar, AssrtAFormula> svars,  // CHECKME: AssrtIntVar?
			AssrtBFormula ass, LinkedHashMap<AssrtVar, AssrtAFormula> phantom)
	{
		return new AssrtEState(labs, svars, ass, phantom);
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

	/*@Override
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
	}*/
	
	@Override
	public AssrtCoreEMsg AssrtCoreEMsg(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass)//, List<AssrtAFormula> sexprs)
			//Map<AssrtIntVarFormula, AssrtIntVarFormula> shadow)
	{
		return new AssrtCoreEMsg(this.mf, peer, mid, pay, ass);//, sexprs);//, shadow);
	}
}
