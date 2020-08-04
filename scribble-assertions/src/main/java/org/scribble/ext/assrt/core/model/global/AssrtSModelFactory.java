package org.scribble.ext.assrt.core.model.global;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.SSingleBuffers;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.model.global.action.AssrtSRecv;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public interface AssrtSModelFactory extends SModelFactory
{

	AssrtSConfig AssrtCoreSConfig(Map<Role, EFsm> P, SSingleBuffers Q,
			Map<Role, Set<AssrtVar>> K,
			Map<Role, Set<AssrtBFormula>> F,
			//Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename
			//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes, 
			Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,

			Map<AssrtVar, DataName> Env

			);
	AssrtSModel AssrtCoreSModel(AssrtCore core, AssrtSGraph graph);
	
	AssrtSSend AssrtCoreSSend(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs);
	AssrtSRecv AssrtCoreSRecv(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs);
}

/*
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
	// TODO
 
	AssrtCoreSReq AssrtCoreSReq(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs);
	AssrtCoreSAcc AssrtCoreSAcc(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs);
 */