package org.scribble.ext.assrt.core.type.session.global;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.name.Substitutions;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtEnd;
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
