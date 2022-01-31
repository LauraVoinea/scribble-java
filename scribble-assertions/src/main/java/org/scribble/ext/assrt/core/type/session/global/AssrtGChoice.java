package org.scribble.ext.assrt.core.type.session.global;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
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
import org.scribble.ext.assrt.core.type.formula.AssrtBinBFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtFormulaFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtTrueFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtActionKind;
import org.scribble.ext.assrt.core.type.session.AssrtChoice;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.AssrtSTypeFactory;
import org.scribble.ext.assrt.core.type.session.AssrtSyntaxException;
import org.scribble.ext.assrt.core.type.session.local.AssrtLActionKind;
import org.scribble.ext.assrt.core.type.session.local.AssrtLChoice;
import org.scribble.ext.assrt.core.type.session.local.AssrtLEnd;
import org.scribble.ext.assrt.core.type.session.local.AssrtLRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;
import org.scribble.ext.assrt.core.type.session.local.AssrtLTypeFactory;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;

// TODO: rename directed choice
public class AssrtGChoice extends AssrtChoice<Global, AssrtGType>
		implements AssrtGType
{
	public final Role src;
	public final Role dst;  // this.dst == super.role

	protected AssrtGChoice(CommonTree source, Role src,
			AssrtGActionKind kind, Role dst,
			LinkedHashMap<AssrtMsg, AssrtGType> cases)
	{
		super(source, dst, kind, cases);
		this.src = src;
		this.dst = dst;
	}

	@Override
	public AssrtGType disamb(AssrtCore core, Map<AssrtVar, DataName> env)
	{
		LinkedHashMap<AssrtMsg, AssrtGType> cases = new LinkedHashMap<>();
		this.cases.entrySet().stream()
				.forEach(x -> cases.put(x.getKey().disamb(env),
						x.getValue().disamb(core, env)));
		return ((AssrtGTypeFactory) core.config.tf.global).AssrtCoreGChoice(
				getSource(), this.src, getKind(), this.dst, cases);
	}

	@Override
	public AssrtGType substitute(AssrtCore core, Substitutions subs)
	{
		Role src = subs.subsRole(this.src);
		Role dst = subs.subsRole(this.dst);
		LinkedHashMap<AssrtMsg, AssrtGType> cases = new LinkedHashMap<>();
		this.cases.entrySet().stream()
				.forEach(x -> cases.put(x.getKey(),
						x.getValue().substitute(core, subs)));
		return ((AssrtGTypeFactory) core.config.tf.global)
				.AssrtCoreGChoice(getSource(), src, getKind(), dst, cases);
	}

	@Override
	public AssrtGType checkDoArgs(AssrtCore core)
	{
		this.cases.entrySet().stream().forEach(x -> x.getValue().checkDoArgs(core));
		return this;
	}

	@Override
	public AssrtGType inline(AssrtGTypeInliner v)
	{
		LinkedHashMap<AssrtMsg, AssrtGType> cases = new LinkedHashMap<>();
		this.cases.entrySet().stream()
				.forEach(x -> cases.put(x.getKey(), x.getValue().inline(v)));
		return ((AssrtGTypeFactory) v.core.config.tf.global)
				.AssrtCoreGChoice(getSource(), this.src, getKind(), this.dst, cases);
	}

	@Override
	public AssrtGType pruneRecs(AssrtCore core)
	{
		LinkedHashMap<AssrtMsg, AssrtGType> pruned = new LinkedHashMap<>();
		for (Entry<AssrtMsg, AssrtGType> e : this.cases.entrySet())
		{
			AssrtGType tmp = e.getValue().pruneRecs(core);
			if (!tmp.equals(AssrtGEnd.END))
			{
				pruned.put(e.getKey(), tmp);
			}
		}
		if (pruned.isEmpty())
		{
			return AssrtGEnd.END;
		}
		return ((AssrtGTypeFactory) core.config.tf.global)
				.AssrtCoreGChoice(getSource(), this.src, getKind(), this.dst, pruned);
	}

	@Override
	public AssrtLType projectInlined(AssrtCore core, Role self,
			AssrtBFormula f, Map<Role, Set<AssrtVar>> known,
			Map<RecVar, LinkedHashMap<AssrtVar, Role>> located,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
			throws AssrtSyntaxException
	{
		AssrtLTypeFactory tf = (AssrtLTypeFactory) core.config.tf.local;
		LinkedHashMap<AssrtMsg, AssrtLType> projs = new LinkedHashMap<>();
		for (Entry<AssrtMsg, AssrtGType> e : this.cases.entrySet())
		{
			AssrtMsg a = e.getKey();
			AssrtBFormula fproj = AssrtFormulaFactory
					.AssrtBinBool(AssrtBinBFormula.Op.And, f, a.ass);

			Map<Role, Set<AssrtVar>> tmp = new HashMap<>(known);
			if (this.src.equals(self) || this.dst.equals(self))
			{
				Set<AssrtVar> tmp2 = new HashSet<>(tmp.get(self));
				tmp.put(self, tmp2);
				a.pay.stream().forEach(x -> tmp2.add(x.var));

				AssrtMsg a1 = ((AssrtSTypeFactory) core.config.tf)
						.AssrtCoreAction(a.op, a.pay, a.ass, phantom, phantAss);
				projs.put(a1,
						e.getValue().projectInlined(core, self, fproj, tmp, located,
								Collections.emptyList(), AssrtTrueFormula.TRUE));
				// N.B. local actions directly preserved from globals -- so core-receive also has assertion (cf. AssrtGMessageTransfer.project, currently no AssrtLReceive)
				// CHECKME: receive assertion projection -- should not be the same as send?
			}
			else
			{
				List<AssrtAnnotDataName> phantom1 = new LinkedList<>(phantom);
				a.pay.stream().forEach(x -> phantom1.add(x));
				AssrtBFormula phantAss1 = phantAss.equals(AssrtTrueFormula.TRUE)
						? (a.ass.equals(AssrtTrueFormula.TRUE)
								? AssrtTrueFormula.TRUE : a.ass)
						: AssrtFormulaFactory
								.AssrtBinBool(AssrtBinBFormula.Op.And, phantAss, a.ass);
				projs.put(a, e.getValue().projectInlined(core, self, fproj, tmp,
						located, phantom1, phantAss1));
			}
		}
		
		// "Simple" cases
		if (this.src.equals(self) || this.dst.equals(self))
		{
			Role role = this.src.equals(self) ? this.dst : this.src;
			AssrtLActionKind k = getKind().project(this.src, self);
			return tf.AssrtCoreLChoice(null, role, k, projs);
		}

		/* "Merge" -- simply disregard phantoms now, assume incorporated downstream (or discarded by end) */

		if (projs.values().stream().anyMatch(v -> (v instanceof AssrtLEnd)))
		{
			if (projs.values().stream()
					.anyMatch(v -> !(v instanceof AssrtLEnd)))
			{
				throw new AssrtSyntaxException("[assrt-core] Cannot project \n"
						+ this + "\n onto " + self + ": cannot merge mixed termination.");
			}
			return AssrtLEnd.END;
		}

		if (projs.values().stream().anyMatch(v -> (v instanceof AssrtLRecVar)))
		{
			if (projs.values().stream()
					.anyMatch(v -> !(v instanceof AssrtLRecVar)))
			{
				throw new AssrtSyntaxException("[assrt-core] Cannot project \n"
						+ this + "\n onto " + self + ": cannot merge unguarded rec vars.");
			}

			Set<RecVar> rvs = projs.values().stream()
					.map(v -> ((AssrtLRecVar) v).recvar).collect(Collectors.toSet());
			Set<List<AssrtAFormula>> fs = projs.values().stream()
					.map(v -> ((AssrtLRecVar) v).stateexprs)
					.collect(Collectors.toSet());
					// CHECKME? syntactic equality of exprs
			if (rvs.size() > 1 || fs.size() > 1)
			{
				throw new AssrtSyntaxException("[assrt-core] Cannot project \n"
						+ this + "\n onto " + self + ": mixed unguarded rec vars: " + rvs);
			}

			return tf.AssrtCoreLRecVar(null, rvs.iterator().next(),
					fs.iterator().next());
		}
		
		List<AssrtLType> filtered = projs.values().stream()
				//.filter(v -> !v.equals(AssrtLEnd.END))
			.collect(Collectors.toList());
	
		/*if (filtered.size() == 0)
		{
			return AssrtLEnd.END;
		}
		else */if (this.cases.size() == 1 && filtered.size() == 1)  // Basic sequencing (unary choice)
		{
			return //(AssrtCoreLChoice)
					filtered.iterator().next();  // RecVar disallowed above
		}
		
		List<AssrtLChoice> choices = filtered.stream()
				.map(v -> (AssrtLChoice) v).collect(Collectors.toList());
	
		Set<Role> roles = choices.stream().map(v -> v.peer)
				.collect(Collectors.toSet());
				// Subj not one of curent src/dest, must be projected inside each case to a guarded continuation
		if (roles.size() > 1)
		{
			throw new AssrtSyntaxException("[assrt-core] Cannot project \n" + this
					+ "\n onto " + self + ": mixed peer roles: " + roles);
		}
		Set<AssrtActionKind<?>> kinds = choices.stream().map(v -> v.kind)
				.collect(Collectors.toSet());
				// Subj not one of curent src/dest, must be projected inside each case to a guarded continuation
		if (kinds.size() > 1)
		{
			throw new AssrtSyntaxException("[assrt-core] Cannot project \n" + this
					+ "\n onto " + self + ": mixed action kinds: " + kinds);
		}
		
		LinkedHashMap<AssrtMsg, AssrtLType> merged = new LinkedHashMap<>();
		choices.forEach(v ->
		{
			if (!v.kind.equals(AssrtLActionKind.RECV))
			{
				throw new RuntimeException("[assrt-core] Shouldn't get here: " + v);  // By role-enabling?
			}
			v.cases.entrySet().forEach(e ->
					{
						AssrtMsg k = e.getKey();
						AssrtLType b = e.getValue();
						//if (merged.containsKey(k))                //&& !b.equals(merged.get(k))) // TODO
						List<AssrtMsg> collect = merged.keySet().stream()
								.filter(x -> x.op.equals(k.op))
								.collect(Collectors.toList());
						if (!collect.isEmpty())
						{
							/*if (!(k.ass.equals(AssrtTrueFormula.TRUE) 
									&& collect.stream().allMatch(x -> x.ass.equals(AssrtTrueFormula.TRUE)))) // Should be singleton ...TODO: check continuations equal*/
							{
								throw new AssrtSyntaxException(
										"[assrt-core] Cannot project \n" + this + "\n onto " + self
												+ ": cannot merge labels of: " + k + " and "
												+ merged.keySet());
							}
						}
						merged.put(k, b);
					});
		});
		
		return tf.AssrtCoreLChoice(null, roles.iterator().next(),
				AssrtLActionKind.RECV, merged);
	}

	@Override
	public List<AssrtAnnotDataName> collectAnnotDataVarDecls(Map<AssrtVar, DataName> env)
	{
		List<AssrtAnnotDataName> res = this.cases.keySet().stream()
				.flatMap(a -> a.pay.stream()).collect(Collectors.toList());
		for (AssrtMsg m : this.cases.keySet()) {
			Map<AssrtVar, DataName> tmp = new HashMap<>(env);
			m.pay.forEach(x -> tmp.put(x.var, x.data));
			res.addAll(this.cases.get(m).collectAnnotDataVarDecls(tmp));
		}
		return res;
	}
	
	@Override
	public AssrtGActionKind getKind()
	{
		return (AssrtGActionKind) this.kind;
	}

	@Override
	public String toString()
	{
		return this.src.toString() + this.kind + this.dst + casesToString();  // toString needed?
	}
	
	@Override
	public int hashCode()
	{
		int hash = 2339;
		hash = 31 * hash + super.hashCode();  // Does this.dst/super.role

		hash = 31 * hash + this.src.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object obj)
	{
		if (this == obj)
		{
			return true;
		}
		if (!(obj instanceof AssrtGChoice))
		{
			return false;
		}
		return super.equals(obj)  // Checks canEquals and this.dst/super.role
				&& this.src.equals(((AssrtGChoice) obj).src);  
	}
	
	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtGChoice;
	}
}
