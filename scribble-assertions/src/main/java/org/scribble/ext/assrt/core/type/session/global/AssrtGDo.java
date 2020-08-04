/**
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */
package org.scribble.ext.assrt.core.type.session.global;

import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.job.Core;
import org.scribble.core.lang.SubprotoSig;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.name.Substitutions;
import org.scribble.core.type.session.Arg;
import org.scribble.ext.assrt.core.job.AssrtCore;
import org.scribble.ext.assrt.core.lang.global.AssrtGProtocol;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtDo;
import org.scribble.ext.assrt.core.type.session.AssrtSyntaxException;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;
import org.scribble.ext.assrt.core.visit.global.AssrtGTypeInliner;

public class AssrtGDo extends AssrtDo<Global, AssrtGType>
		implements AssrtGType
{
	protected AssrtGDo(CommonTree source, ProtoName<Global> proto,
			List<Role> roles, List<Arg<? extends NonRoleParamKind>> args,
			List<AssrtAFormula> sexprs)
	{
		super(source, proto, roles, args, sexprs);
	}

	// Currently unused? disamb done on inlined -- cf. AssrtCoreContext.getInlined
	@Override
	public AssrtGType disamb(AssrtCore core, Map<AssrtVar, DataName> env)
	{
		return ((AssrtGTypeFactory) core.config.tf.global).AssrtCoreGDo(
				getSource(), this.proto, this.roles, this.args,
				this.stateexprs.stream().map(x -> (AssrtAFormula) x.disamb(env))
						.collect(Collectors.toList()));
	}

	@Override
	public AssrtGType substitute(AssrtCore core, Substitutions subs)
	{
		//throw new RuntimeException( "[TODO] Substitutions for " + getClass() + ":\n" + this);
		List<Role> roles = this.roles.stream().map(x -> subs.subsRole(x, false))
				.collect(Collectors.toList());
		if (!this.args.isEmpty())
		{
			throw new RuntimeException(
					"[assrt-core] [TODO] Non-role do-args:\n\t" + this);
		}
		return ((AssrtGTypeFactory) core.config.tf.global).AssrtCoreGDo(
				getSource(), this.proto, roles, this.args, this.stateexprs);
	}

	@Override
	public AssrtGType inline(AssrtGTypeInliner v)
	{
		ProtoName<Global> fullname = this.proto;
		SubprotoSig sig = new SubprotoSig(fullname, this.roles, this.args);
		RecVar rv = v.getInlinedRecVar(sig);
		AssrtGTypeFactory tf = (AssrtGTypeFactory) v.core.config.tf.global;
		if (v.hasSig(sig))
		{
			return tf.AssrtCoreGRecVar(getSource(), rv, this.stateexprs);
		}
		v.pushSig(sig);
		AssrtGProtocol gpro = getTarget(v.core);
		if (!gpro.params.isEmpty() || !this.args.isEmpty())
		{
			throw new RuntimeException("[TODO] Inlining for proto/do params/args for "
					+ getClass() + ":\n" + this);
		}
		Substitutions subs = new Substitutions(gpro.roles, this.roles, gpro.params,
				this.args);
		AssrtGType inlined = gpro.type.substitute((AssrtCore) v.core, subs)  // N.B. .type, not .def
				.inline(v);  // Cf. GTypeInliner.visitDo -- recursive visit subproto

		v.popSig();
		// "Inlining" action sexprs as target rec svar-exprs
		LinkedHashMap<AssrtVar, AssrtAFormula> svars = new LinkedHashMap<>();
		Iterator<AssrtAFormula> sexprs = this.stateexprs.iterator();
		gpro.statevars.keySet().forEach(x -> svars.put(x, sexprs.next()));  // gpro.statevars keyset is ordered
				// Do-inlining is implicitly a f/w entry: also "inline" (i.e., replace) target svar exprs by this.sexprs -- sexprs o/w only carried by recvar (which f/w entry is not) 
		// Cf. AssrtCoreEGraphBuilder.buildEdgeAndContinuation, AssrtCoreLRec `cont` f/w rec
		LinkedHashMap<AssrtVar, AssrtAFormula> phantom = new LinkedHashMap<>();
		gpro.statevars.keySet().stream().filter(x -> gpro.located.get(x) != null)
				.forEach(x -> phantom.put(x, gpro.statevars.get(x)));
		LinkedHashMap<AssrtVar, Role> located = new LinkedHashMap<>();
		gpro.located.entrySet().stream()
				.forEach(x -> located.put(x.getKey(),
						x.getValue() == null ? null : subs.subsRole(x.getValue())));
		return tf.AssrtCoreGRec(null, rv, inlined, svars, gpro.assertion,
				located, phantom);
	}

	@Override
	public AssrtGType pruneRecs(AssrtCore core)
	{
		return this;
	}

	@Override
	public AssrtLType projectInlined(AssrtCore core, Role self,
			AssrtBFormula f, Map<Role, Set<AssrtVar>> known,
			Map<RecVar, LinkedHashMap<AssrtVar, Role>> located,
			List<AssrtAnnotDataName> phantom, AssrtBFormula phantAss)
			throws AssrtSyntaxException
	{
		throw new RuntimeException("[TODO] :\n\t" + this);
	}

	@Override
	public List<AssrtAnnotDataName> collectAnnotDataVarDecls(
			Map<AssrtVar, DataName> env)
	{
		return Collections.emptyList();
	}
	
	@Override
	public AssrtGProtocol getTarget(Core core)
	{
		return ((AssrtCore) core).getContext().getIntermediate(this.proto);  // Subproto visiting hardcoded to intermediate (i.e., parsed)
	}

	@Override
	public int hashCode()
	{
		int hash = 24917;
		hash = 31 * hash + super.hashCode();
		return hash;
	}

	@Override
	public boolean equals(Object o)
	{
		if (this == o)
		{
			return true;
		}
		if (!(o instanceof AssrtGDo))
		{
			return false;
		}
		return super.equals(o);  // Does canEquals
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof AssrtGDo;
	}
}







	
	
	
	
	
	
	
	
	
	

	/*
	@Override
	public GProtoName getProto()
	{
		return (GProtoName) this.proto;
	}
 
	// CHECKME: factor up to base?
	@Override
	public GType getInlined(STypeInliner v)
	{
		GProtocolName fullname = this.proto;
		SubprotoSig sig = new SubprotoSig(fullname, this.roles, this.args);
		RecVar rv = v.getInlinedRecVar(sig);
		if (v.hasSig(sig))
		{
			return new GContinue(getSource(), rv);
		}
		v.pushSig(sig);
		GProtocol g = v.job.getContext().getIntermediate(fullname);
		Substitutions subs = 
				new Substitutions(g.roles, this.roles, g.params, this.args);
		GSeq inlined = g.def.substitute(subs).getInlined(v);
				// i.e. returning a GSeq -- rely on parent GSeq to inline
		v.popSig();
		return new GRecursion(null, rv, inlined);
	}
	
	@Override
	public Set<Role> checkRoleEnabling(Set<Role> enabled) throws ScribException
	{
		throw new RuntimeException("Unsupported for Do: " + this);
	}

	@Override
	public Map<Role, Role> checkExtChoiceConsistency(Map<Role, Role> enablers)
			throws ScribException
	{
		throw new RuntimeException("Unsupported for Do: " + this);
	}

	@Override
	public LType projectInlined(Role self)
	{
		throw new RuntimeException("Unsupported for Do: " + this);
	}

	@Override
	public LType project(ProjEnv v)
	{
		if (!this.roles.contains(v.self))
		{
			return LSkip.SKIP;
		}

		JobContext jobc = v.job.getContext();
		//GProtocolDecl gpd = jobc.getParsed(this.proto);
		GProtocol gpd = jobc.getIntermediate(this.proto);
		Role targSelf = gpd.roles.get(this.roles.indexOf(v.self));

		GProtocol imed = jobc.getIntermediate(this.proto);
		if (!imed.roles.contains(targSelf))  // CHECKME: because roles already pruned for intermed decl?
		{
			return LSkip.SKIP;
		}

		LProtocolName fullname = ProjEnv.projectFullProtocolName(this.proto,
				targSelf);
		Substitutions subs = new Substitutions(imed.roles, this.roles,
				Collections.emptyList(), Collections.emptyList());
		List<Role> used = jobc.getInlined(this.proto).roles.stream()
				.map(x -> subs.subsRole(x)).collect(Collectors.toList());
		List<Role> rs = this.roles.stream().filter(x -> used.contains(x))
				.map(x -> x.equals(v.self) ? Role.SELF : x)
						// CHECKME: "self" also explcitily used for Choice, but implicitly for MessageTransfer, inconsistent?
				.collect(Collectors.toList());
		return new LDo(null, fullname, rs, this.args);  // TODO CHECKME: prune args?
	}
	*/










