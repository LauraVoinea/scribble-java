package org.scribble.ext.assrt.core.model.endpoint;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreERecv;
import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreESend;
import org.scribble.ext.assrt.core.model.global.AssrtCoreEMsg;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public interface AssrtCoreEModelFactory extends EModelFactory
{

	AssrtEState newAssrtEState(Set<RecVar> labs,
			LinkedHashMap<AssrtVar, AssrtAFormula> svars,
			AssrtBFormula ass, LinkedHashMap<AssrtVar, AssrtAFormula> phantom);

	AssrtCoreESend AssrtCoreESend(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs,
			//LinkedHashMap<AssrtIntVar, AssrtAFormula> phantoms,
			List<AssrtAnnotDataName> phantoms,
			AssrtBFormula phantAss);
	AssrtCoreERecv AssrtCoreERecv(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs,
			//LinkedHashMap<AssrtIntVar, AssrtAFormula> phantoms,
			List<AssrtAnnotDataName> phantoms,
			AssrtBFormula phantAss);
	
	AssrtCoreEMsg AssrtCoreEMsg(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass);// List<AssrtAFormula> sexprs);
			//Map<AssrtIntVarFormula, AssrtIntVarFormula> shadow);
}

/*
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
	AssrtCoreEReq AssrtCoreEReq(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs);
	AssrtCoreEAcc AssrtCoreEAcc(Role peer, MsgId<?> mid, Payload pay,
			AssrtBFormula ass, List<AssrtAFormula> sexprs);
*/