package org.scribble.ext.assrt.core.type.session.global;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.stream.Collectors;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.name.Substitutions;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLTypeFactory;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;

	
public class AssrtGRecVar extends AssrtRecVar<Global, AssrtGType>
		implements AssrtGType
{
	protected AssrtGRecVar(CommonTree source, RecVar rv,
			List<AssrtAFormula> sexprs)
	{
		super(source, rv, sexprs);
	}

	@Override
	public AssrtGType disamb(AssrtCore core, Map<AssrtVar, DataName> env)
	{
		return new AssrtGRecVar(getSource(), this.recvar,
				this.stateexprs.stream().map(x -> (AssrtAFormula) x.disamb(env))
						.collect(Collectors.toList()));
	}

	@Override
	public AssrtGType substitute(AssrtCore core, Substitutions subs)
	{
		return this;
	}

	@Override
	public AssrtGType checkDoArgs(AssrtCore core)
	{
		throw new RuntimeException(
				"[assrt] TODO: AssrtGRecVar.checkDoArgs\n\t" + this);
	}

	@Override
	public AssrtGType inline(AssrtGTypeInliner v)
	{
		RecVar rv = v.getInlinedRecVar(this.recvar);
		return ((AssrtGTypeFactory) v.core.config.tf.global)
				.AssrtCoreGRecVar(getSource(), rv, this.stateexprs);
	}

	@Override
	public AssrtGType pruneRecs(AssrtCore core)
	{
		return this;
	}

	@Override
	public AssrtLRecVar projectInlined(AssrtCore core, Role self,
			AssrtBFormula f, Map<Role, Set<AssrtVar>> known,
			Map<RecVar, LinkedHashMap<AssrtVar, Role>> located,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)  // CHECKME: phantoms discarded for recvar?
	{
		Iterator<Entry<AssrtVar, Role>> it = located.get(this.recvar).entrySet()
				.iterator();
		List<AssrtAFormula> sexprs = this.stateexprs.stream()
				.filter(x ->
					{
						Role r = it.next().getValue();
						return r == null || r.equals(self);
					})
				.collect(Collectors.toList());
		return ((AssrtLTypeFactory) core.config.tf.local).AssrtCoreLRecVar(null,
				this.recvar, sexprs);
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
		if (!(obj instanceof AssrtGRecVar))
		{
			return false;
		}
		return super.equals(obj);  // Does canEquals
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtGRecVar;
	}

	@Override
	public int hashCode()
	{
		int hash = 2411;
		hash = 31*hash + super.hashCode();
		return hash;
	}
}
