package org.scribble.ext.assrt.core.type.session.global;

import java.util.*;

import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.*;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.model.global.AssrtSModelFactory;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtEnd;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGConfig;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGEnv;
import org.scribble.ext.assrt.core.type.session.local.AssrtLEnd;
import org.scribble.ext.assrt.core.type.session.local.AssrtLTypeFactory;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;


public class AssrtGEnd extends AssrtEnd<Global, AssrtGType>
		implements AssrtGType
{
	public static final AssrtGEnd END = new AssrtGEnd();
	
	protected AssrtGEnd()
	{
		
	}

	@Override
	public AssrtGType disamb(AssrtCore core, Map<AssrtVar, DataName> env)
	{
		return this;
	}

	@Override
	public AssrtGType substitute(AssrtCore core, Substitutions subs)
	{
		return this;
	}

	@Override
	public AssrtGType checkDoArgs(AssrtCore core)
	{
		return this;
	}

	@Override
	public AssrtGType inline(AssrtGTypeInliner v)
	{
		return this;
	}

	@Override
	public AssrtGType pruneRecs(AssrtCore core)
	{
		return this;
	}

	@Override
	public AssrtLEnd projectInlined(AssrtCore core, Role self,
			AssrtBFormula f, Map<Role, Set<AssrtVar>> known,
			Map<RecVar, LinkedHashMap<AssrtVar, Role>> located,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
	{
		return ((AssrtLTypeFactory) core.config.tf.local).AssrtCoreLEnd();
	}

	@Override
	public List<AssrtAnnotDataName> collectAnnotDataVarDecls(
			Map<AssrtVar, DataName> env)
	{
		return Collections.emptyList();
	}


	@Override
	public AssrtGType unfold(AssrtGTypeFactory gf, RecVar rv, AssrtGType body) {
		return this;
	}

	@Override
	public Map<Role, Set<AssrtSSend>> collectImmediateActions(
			AssrtSModelFactory mf, Map<Role, Set<AssrtSSend>> env)
	{
		return env;
	}

	@Override
	public Optional<AssrtGConfig> step(
			AssrtGTypeFactory gf, AssrtGEnv gamma, AssrtSSend action) {
		throw new RuntimeException("Not defined for `end`");
	}

	@Override
	public boolean equals(Object obj)
	{
		if (!(obj instanceof AssrtGEnd))
		{
			return false;
		}
		return super.equals(obj);  // Checks canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtGEnd;
	}

	@Override
	public int hashCode()
	{
		return 31*2381;
	}
}
