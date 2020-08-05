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
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtERecv;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtESend;
import org.scribble.ext.assrt.core.model.global.AssrtEMsg;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public class AssrtEModelFactoryImpl extends EModelFactoryImpl//AssrtEModelFactoryImpl
		implements AssrtEModelFactory
{

	public AssrtEModelFactoryImpl(ModelFactory mf)
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
	public AssrtESend AssrtESend(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		return new AssrtESend(this.mf, peer, mid, pay, ass, sexprs, phantom,
				phantAss);
	}

	@Override
	public AssrtERecv AssrtERecv(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		return new AssrtERecv(this.mf, peer, mid, pay, ass, sexprs, phantom,
				phantAss);
	}

	/*@Override
	public AssrtEReq AssrtEReq(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		//return new AssrtEReq(this.mf, peer, mid, pay, ass, sexprs);
		throw new RuntimeException("TODO");
	}
	
	@Override
	public AssrtEAcc AssrtEAcc(Role peer, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		//return new AssrtEAcc(this.mf, peer, mid, pay, ass, sexprs);
		throw new RuntimeException("TODO");
	}*/
	
	@Override
	public AssrtEMsg AssrtEMsg(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass)//, List<AssrtAFormula> sexprs)
			//Map<AssrtIntVarFormula, AssrtIntVarFormula> shadow)
	{
		return new AssrtEMsg(this.mf, peer, mid, pay, ass);//, sexprs);//, shadow);
	}
}
