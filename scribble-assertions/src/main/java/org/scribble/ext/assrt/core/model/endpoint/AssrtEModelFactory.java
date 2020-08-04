package org.scribble.ext.assrt.core.model.endpoint;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtERecv;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtESend;
import org.scribble.ext.assrt.core.model.global.AssrtEMsg;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public interface AssrtEModelFactory extends EModelFactory
{

	AssrtEState newAssrtEState(Set<RecVar> labs,
			LinkedHashMap<AssrtVar, AssrtAFormula> svars,
			AssrtBFormula ass, LinkedHashMap<AssrtVar, AssrtAFormula> phantom);

	AssrtESend AssrtCoreESend(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs,
			//LinkedHashMap<AssrtIntVar, AssrtAFormula> phantoms,
			List<AssrtAnnotDataName> phantoms,
			AssrtBFormula phantAss);
	AssrtERecv AssrtCoreERecv(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs,
			//LinkedHashMap<AssrtIntVar, AssrtAFormula> phantoms,
			List<AssrtAnnotDataName> phantoms,
			AssrtBFormula phantAss);
	
	AssrtEMsg AssrtCoreEMsg(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass);// List<AssrtAFormula> sexprs);
			//Map<AssrtIntVarFormula, AssrtIntVarFormula> shadow);
}

/*
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
	AssrtCoreEReq AssrtCoreEReq(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs);
	AssrtCoreEAcc AssrtCoreEAcc(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs);
*/