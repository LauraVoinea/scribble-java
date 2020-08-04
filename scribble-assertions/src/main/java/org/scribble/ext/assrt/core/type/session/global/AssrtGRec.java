package org.scribble.ext.assrt.core.type.session.global;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
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
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtRec;
import org.scribble.ext.assrt.core.type.session.AssrtSyntaxException;
import org.scribble.ext.assrt.core.type.session.local.AssrtLEnd;
import org.scribble.ext.assrt.core.type.session.local.AssrtLRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;
import org.scribble.ext.assrt.core.type.session.local.AssrtLTypeFactory;
import org.scribble.ext.assrt.core.visit.gather.AssrtRecVarGatherer;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;

public class AssrtGRec extends AssrtRec<Global, AssrtGType>
		implements AssrtGType
{
	// Pre: same keys as super.statevars
	public final LinkedHashMap<AssrtVar, Role> located;  // maps to null for "global" (back compat)  // TODO: sorts (currently hardcoded around int)
	public final LinkedHashMap<AssrtVar, AssrtAFormula> phantom;  // Record init exprs for potential local phantoms, e.g., for forwards-do

	protected AssrtGRec(CommonTree source, RecVar rv, AssrtGType body,
			LinkedHashMap<AssrtVar, AssrtAFormula> svars, AssrtBFormula ass,
			LinkedHashMap<AssrtVar, Role> located,
			LinkedHashMap<AssrtVar, AssrtAFormula> phantom)
	{
		super(source, rv, body, svars, ass);
		this.located = new LinkedHashMap<>(located);
		this.phantom = new LinkedHashMap<>(phantom);
	}

	@Override
	public AssrtGType disamb(AssrtCore core, Map<AssrtVar, DataName> env)
	{
		Map<AssrtVar, DataName> env1 = new HashMap<>(env);
		this.statevars.entrySet()
				.forEach(x -> env1.put(x.getKey(), x.getValue().getSort(env1)));
		LinkedHashMap<AssrtVar, AssrtAFormula> svars = new LinkedHashMap<>();
		this.statevars.entrySet().forEach(x -> svars.put(x.getKey(),
				(AssrtAFormula) x.getValue().disamb(env1)));  // Unnecessary, disallow mutual var refs?
		this.phantom.entrySet()
				.forEach(x -> env1.put(x.getKey(), x.getValue().getSort(env1)));
		LinkedHashMap<AssrtVar, AssrtAFormula> phantom = new LinkedHashMap<>();
		this.phantom.entrySet().forEach(x -> phantom.put(x.getKey(),
				(AssrtAFormula) x.getValue().disamb(env1)));
		return ((AssrtGTypeFactory) core.config.tf.global).AssrtCoreGRec(
				getSource(), this.recvar, this.body.disamb(core, env1), svars,
				(AssrtBFormula) this.assertion.disamb(env1), this.located, phantom);
	}

	@Override
	public AssrtGType substitute(AssrtCore core, Substitutions subs)
	{
		return ((AssrtGTypeFactory) core.config.tf.global).AssrtCoreGRec(
				getSource(), this.recvar, this.body.substitute(core, subs), this.statevars,
				this.assertion, this.located, this.phantom);
	}

	@Override
	public AssrtGType inline(AssrtGTypeInliner v)
	{
		throw new RuntimeException("[TODO] :\n" + this);
	}

	@Override
	public AssrtGType pruneRecs(AssrtCore core)
	{
		Set<RecVar> rvs = this.body
				.assrtCoreGather(  // TODO: factor out with base gatherer
						new AssrtRecVarGatherer<Global, AssrtGType>()::visit)
				.collect(Collectors.toSet());
		return rvs.contains(this.recvar) ? this : this.body;
	}

	@Override
	public AssrtLType projectInlined(AssrtCore core, Role self,
			AssrtBFormula f, Map<Role, Set<AssrtVar>> known,
			Map<RecVar, LinkedHashMap<AssrtVar, Role>> located,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
			throws AssrtSyntaxException
	{
		Map<RecVar, LinkedHashMap<AssrtVar, Role>> tmp = new HashMap<>(located);
		tmp.put(this.recvar, this.located);

		LinkedHashMap<AssrtVar, AssrtAFormula> svars = new LinkedHashMap<>();
		LinkedHashMap<AssrtVar, AssrtAFormula> phantomSVars = new LinkedHashMap<>();
		/*this.statevars.entrySet().stream()  // ordered
				.filter(x ->
					{
						Role r = this.located.get(x.getKey());
						return r == null || r.equals(self);
					})
				.forEach(x -> svars.put(x.getKey(), x.getValue()));*/
		for (Entry<AssrtVar, AssrtAFormula> e : this.statevars.entrySet())
		{
			AssrtVar v = e.getKey();
			Role r = this.located.get(v);
			if (r == null || r.equals(self))
			{
				AssrtAFormula a = e.getValue();
				svars.put(v, a);
			}
			else
			{
				AssrtAFormula a = this.phantom.get(v);
				phantomSVars.put(v, a);
			}
		}

		Map<Role, Set<AssrtVar>> tmp2 = new HashMap<>(known);
		Set<AssrtVar> tmp3 = tmp2.get(self);
		tmp3.addAll(svars.keySet());  // Agnostic to shadowing -- cf. AssrtCoreGProtocol and inserted top-level rec
		tmp3.addAll(phantomSVars.keySet());
		tmp2.put(self, tmp3);

		AssrtLType proj = this.body.projectInlined(core, self, f, tmp2, tmp,
				phantom, phantAss);  // CHECKME: "reordering" of phantom/phantAss and phantomSVars

		Set<AssrtVar> assVars = this.assertion.getIntVars();
		Set<AssrtVar> k = known.get(self);
		AssrtBFormula ass = this.assertion;
		if (!k.containsAll(assVars))  // FIXME: phantoms -- HERE: treat phantoms as known, i.e., assvars not really "projected" any more  // similarly in model building
		{
			assVars.retainAll(k);
			if (!assVars.isEmpty())
			{
				throw new AssrtSyntaxException(
						"Cannot project assertion onto " + self
								+ ", some (but not all) variables unknown: "
								+ assVars + "\n\t" + this);
				// HACK FIXME: cf. model K (do in model checking?)
			}
			ass = AssrtTrueFormula.TRUE;
		}

		return (proj instanceof AssrtLRecVar) 
				? AssrtLEnd.END
				: ((AssrtLTypeFactory) core.config.tf.local)
						.AssrtCoreLRec(null, this.recvar, svars, proj, ass, phantomSVars);
	}

	@Override
	public List<AssrtAnnotDataName> collectAnnotDataVarDecls(
			Map<AssrtVar, DataName> env)
	{
		List<AssrtAnnotDataName> res = new LinkedList<>();
		Map<AssrtVar, DataName> env1 = new HashMap<>(env);
		this.statevars.entrySet()
				.forEach(x -> env1.put(x.getKey(), x.getValue().getSort(env1)));

		this.statevars.keySet().stream().forEachOrdered(
				v -> res.add(new AssrtAnnotDataName(v, env1.get(v))));
		/*this.ass.getIntVars().stream().forEachOrdered(
				v -> res.add(new AssrtAnnotDataType(v, new DataType("int"))));  // No: not decls*/

		res.addAll(this.body.collectAnnotDataVarDecls(env1));
		return res;
	}

	@Override
	public String toString()
	{
		return "mu " + this.recvar + "<"
				+ this.statevars.entrySet().stream()
						.map(x -> x.getKey()
								+ (this.located.get(x.getKey()) == null  // Cf. AssrtCoreGProtocol
										? " :"
										: ":" + this.located.get(x.getKey()) + " ")
								+ "= " + x.getValue())
						.collect(Collectors.joining(", "))
				+ ">["
				+ this.phantom.entrySet().stream()
						.map(x -> x.getKey() + ":=" + x.getValue())
						.collect(Collectors.joining(", "))
				+ "]"
				+ this.assertion + "." + this.body;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtGRec))
		{
			return false;
		}
		AssrtGRec them = (AssrtGRec) obj;
		return super.equals(obj)  // Does canEquals
				&& this.located.equals(them.located)
				&& this.phantom.equals(them.phantom);
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtGRec;
	}
	
	@Override
	public int hashCode()
	{
		int hash = 2333;
		hash = 31 * hash + super.hashCode();
		hash = 31 * hash + this.located.hashCode();
		hash = 31 * hash + this.phantom.hashCode();
		return hash;
	}
}
