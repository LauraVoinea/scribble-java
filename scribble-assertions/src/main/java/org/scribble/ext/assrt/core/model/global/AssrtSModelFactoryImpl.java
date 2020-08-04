package org.scribble.ext.assrt.core.model.global;

import java.util.List;
import java.util.Map;
import java.util.Set;

import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EFsm;
import org.scribble.core.model.global.SConfig;
import org.scribble.core.model.global.SGraph;
import org.scribble.core.model.global.SModelFactoryImpl;
import org.scribble.core.model.global.SSingleBuffers;
import org.scribble.core.model.global.SState;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.model.global.action.AssrtSRecv;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public class AssrtSModelFactoryImpl extends SModelFactoryImpl //AssrtSModelFactoryImpl
		implements AssrtSModelFactory
{

	public AssrtSModelFactoryImpl(ModelFactory mf)
	{
		super(mf);
	}

	@Override
	public AssrtSGraphBuilder SGraphBuilder()
	{
		return new AssrtSGraphBuilder(this.mf);
	}

	@Override
	public AssrtSGraphBuilderUtil SGraphBuilderUtil()
	{
		return new AssrtSGraphBuilderUtil(this.mf);
	}

	@Override
	public AssrtSState SState(SConfig config)
	{
		return new AssrtSState((AssrtSConfig) config);
	}

	@Override
	public AssrtSConfig AssrtCoreSConfig(Map<Role, EFsm> P, SSingleBuffers Q,
			Map<Role, Set<AssrtVar>> K,
			Map<Role, Set<AssrtBFormula>> F, Map<Role, Map<AssrtVar, AssrtAFormula>> V,
			Map<Role, Set<AssrtBFormula>> R,

			Map<AssrtVar, DataName> Env

	)
			//Map<Role, Map<AssrtIntVarFormula, AssrtIntVarFormula>> rename
			//Map<Role, LinkedHashMap<Integer, Set<AssrtIntVar>>> scopes)
	{
		return new AssrtSConfig(this.mf, P, Q, K, F, V, R //rename, scopes
				, Env
				);
	}

	@Override
	public AssrtSGraph SGraph(GProtoName fullname, Map<Integer, SState> states, 
			SState init)
	{
		return new AssrtSGraph(fullname, states, init);
	}

	@Override
	public AssrtSModel SModel(SGraph graph)
	{
		//return new AssrtCoreSModel((AssrtCoreSGraph) graph);
		throw new RuntimeException("Deprecated for " + getClass());
	}

	// FIXME: breaks super pattern, extra core arg -- cf. Core.validateByScribble -- CHECKME: core really needed?
	@Override
	public AssrtSModel AssrtCoreSModel(AssrtCore core, AssrtSGraph graph)
	{
		return new AssrtSModel(core, graph);
	}

	@Override
	public AssrtSSend AssrtCoreSSend(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		return new AssrtSSend(subj, obj, mid, pay, ass, sexprs);
	}

	@Override
	public AssrtSRecv AssrtCoreSRecv(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		return new AssrtSRecv(subj, obj, mid, pay, ass, sexprs);
	}
}

/*
  
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
	// TODO: 
 
	@Override
	public AssrtCoreSReq AssrtCoreSReq(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		return new AssrtCoreSReq(subj, obj, mid, pay, ass, sexprs);
	}

	@Override
	public AssrtCoreSAcc AssrtCoreSAcc(Role subj, Role obj, MsgId<?> mid,
			Payload pay, AssrtBFormula ass, List<AssrtAFormula> sexprs)
	{
		return new AssrtCoreSAcc(subj, obj, mid, pay, ass, sexprs);
	}
*/
