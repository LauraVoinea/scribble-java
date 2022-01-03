package org.scribble.ext.assrt.core.type.session.global;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.kind.PayElemKind;
import org.scribble.core.type.name.*;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.model.global.AssrtSModelFactory;
import org.scribble.ext.assrt.core.model.global.action.AssrtSSend;
import org.scribble.ext.assrt.core.type.formula.*;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.*;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGConfig;
import org.scribble.ext.assrt.core.type.session.global.lts.AssrtGEnv;
import org.scribble.ext.assrt.core.type.session.local.*;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;

import java.util.*;
import java.util.Map.Entry;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
				.map(v -> (AssrtLChoice) v).collect(Collectors.toList());  // !!! FIXME: third-party could be `end` or recvar
	
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

	// Pre: no null in range of map
	private static Map<Role, Set<AssrtSSend>> fooCopy(Map<Role, Set<AssrtSSend>> env) {
		return env.entrySet().stream().collect(Collectors.toMap(
				Entry::getKey,
				x -> new HashSet(x.getValue())  // assume not null
		));
	}


	@Override
	public AssrtGType unfold(AssrtGTypeFactory gf, RecVar rv, AssrtGType body) {
		LinkedHashMap<AssrtMsg, AssrtGType> cases_ = new LinkedHashMap<>();
		for (Entry<AssrtMsg, AssrtGType> e : this.cases.entrySet()) {
			cases_.put(e.getKey(), e.getValue().unfold(gf, rv, body));
		}
		return gf.AssrtCoreGChoice(null, this.src,
				AssrtGActionKind.MSG_TRANSFER, this.dst, cases_);
	}

	private static void addMaybeNull(Map<Role, Set<AssrtSSend>> env, Role r, AssrtSSend action) {
		Set<AssrtSSend> acts = env.get(r);
		if (acts == null) {
			acts = new HashSet<>();
			env.put(r, acts);
		}
		acts.add(action);
	}

	private static AssrtSSend msgToSSnd(
			AssrtSModelFactory mf, Role src, Role dst, AssrtMsg m) {
		Payload pay = new Payload(m.pay.stream()
				.map(x -> (PayElemType<?>) x).collect(Collectors.toList()));  // !!! CHECKME: awakward payload construct param type?
		return mf.AssrtCoreSSend(src, dst, m.op, pay, m.ass, Collections.emptyList());
	}

	@Override
	public Map<Role, Set<AssrtSSend>> collectImmediateActions(
			AssrtSModelFactory sf, Map<Role, Set<AssrtSSend>> env) {
		if (this.kind != AssrtGActionKind.MSG_TRANSFER) {
			throw new RuntimeException("TODO: " + this.kind);
		}

		Set<Role> prev = env.keySet();
		Map<Role, Set<AssrtSSend>> add = new HashMap<>();

		List<Map<Role, Set<AssrtSSend>>> nested = new LinkedList<>();
		// !!! synchronous semantics: if either one src/dst already has a pending action, then this current action is blocked
		for (Entry<AssrtMsg, AssrtGType> c : this.cases.entrySet()) {
			Map<Role, Set<AssrtSSend>> env_ = fooCopy(env);
			if (!prev.contains(this.src) && !prev.contains(this.dst)) {
				AssrtMsg m = c.getKey();
				AssrtSSend action = msgToSSnd(sf, this.src, this.dst, m);
				addMaybeNull(add, this.src, action);
				addMaybeNull(env_, this.src, action);
				addMaybeNull(add, this.dst, action);
				addMaybeNull(env_, this.dst, action);
			}
			nested.add(c.getValue().collectImmediateActions(sf, env_));
		}
		if (!nested.isEmpty()) {
			Map<Role, Set<AssrtSSend>> reduce = nested.stream().skip(1)
					.reduce(nested.get(0), AssrtGChoice::retain);
			for (Entry<Role, Set<AssrtSSend>> e : reduce.entrySet()) {
				Role r = e.getKey();
				Set<AssrtSSend> coll = e.getValue();
				Set<AssrtSSend> acts = add.get(r);
				if (acts == null) {
					add.put(r, coll);
				} else {
					acts.addAll(coll);
				}
			}
		}
		return add;
	}

	private static Map<Role, Set<AssrtSSend>> retain(
			Map<Role, Set<AssrtSSend>> m1, Map<Role, Set<AssrtSSend>> m2) {
		Map<Role, Set<AssrtSSend>> res = new HashMap<>();
		for (Role r1 : m1.keySet()) {
			if (m2.containsKey(r1))	{
				Set<AssrtSSend> as1	= m1.get(r1);
				as1.retainAll(m2.get(r1));
				res.put(r1, as1);
			}
		}
		return res;
	}

	// Pre: action \in collectImmediateCases
	@Override
	public Optional<AssrtGConfig> step(
			AssrtGTypeFactory gf, AssrtGEnv gamma, AssrtSSend action)
	{
		if (this.kind != AssrtGActionKind.MSG_TRANSFER) {
			throw new RuntimeException("TODO: " + this.kind);
		}

		if (this.src.equals(action.subj) && this.dst.equals(action.obj)) {
			List<PayElemType<? extends PayElemKind>> elems = action.payload.elems;
			if (elems.size() != 1)
			{
				throw new RuntimeException("TODO: " + action);
			}
			AssrtAnnotDataName pay = (AssrtAnnotDataName) elems.get(0);  // FIXME !!!
			Op op = (Op) action.mid;
			// !!! FIXME: factor out general AssrtMsg (cases keyset) to AssrtSSend
			AssrtMsg msg = new AssrtMsg(op, Stream.of(pay).collect(Collectors.toList()),
					action.ass, null, null);  // !!! Cf. AssrtMsg, null for globals
			if (this.cases.keySet().contains(msg))
			{
				Set<Role> pq = Stream.of(this.src, this.dst).collect(Collectors.toSet());
				AssrtGEnv gamma_ = gamma.extend(pay.var, pq, pay.data, action.ass);
				return Optional.of(new AssrtGConfig(gamma_, this.cases.get(msg)));
			}
			else
			{
				throw new RuntimeException("Undefined:\n\tcases=" + this.cases
						+ "\n\taction=" + action);
			}
		}
		else
		{
			Set<Role> pq = Stream.of(action.subj, action.obj).collect(Collectors.toSet());
			if (!pq.contains(this.src) && !pq.contains(this.dst))  // action.subj \not\in {src, dst}
			{
				List<AssrtGConfig> nested = new LinkedList<>();
				for (Entry<AssrtMsg, AssrtGType> e : this.cases.entrySet()) {  // cf. LinkedHashMap
					AssrtMsg m = e.getKey();
					AssrtGType c = e.getValue();
					if (m.pay.size() > 1) {
						throw new RuntimeException("TODO: " + m);
					}
					AssrtGEnv gamma_ = gamma;
					if (!m.pay.isEmpty()) {
						AssrtAnnotDataName annot = m.pay.get(0);
						gamma_ = gamma_.extend(annot.var,
								Collections.emptySet(), annot.data, m.ass);
					}
					c.step(gf, gamma_, action).map(x -> nested.add(x));
				}
				if (nested.size() != this.cases.size())  // Some case was not executed
				{
					return Optional.empty();
				}
				List<AssrtGEnv> distinct = nested.stream().map(x -> x.gamma)
						.distinct().collect(Collectors.toList());
				if (distinct.size() != 1) {
					return Optional.empty();
				}
				LinkedHashMap<AssrtMsg, AssrtGType> cases_
						= new LinkedHashMap<>();
				Iterator<AssrtGConfig> i = nested.iterator();
				this.cases.keySet().forEach(x -> cases_.put(x, i.next().type));
				AssrtGChoice res = gf.AssrtCoreGChoice(null, this.src,
						AssrtGActionKind.MSG_TRANSFER, this.dst, cases_);
				return Optional.of(new AssrtGConfig(distinct.get(0), res));
			} else {
				throw new RuntimeException("Undefined:\n\tcases=" + this.cases
						+ "\n\taction=" + action);
			}
		}
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
