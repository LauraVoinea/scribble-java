package org.scribble.lang.global;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.scribble.job.ScribbleException;
import org.scribble.lang.Do;
import org.scribble.lang.STypeInliner;
import org.scribble.lang.Substitutions;
import org.scribble.lang.local.LType;
import org.scribble.type.SubprotoSig;
import org.scribble.type.kind.Global;
import org.scribble.type.name.GProtocolName;
import org.scribble.type.name.RecVar;
import org.scribble.type.name.Role;

public class GDo extends Do<Global, GProtocolName> implements GType
{
	public GDo(org.scribble.ast.Do<Global> source, GProtocolName proto,
			List<Role> roles)
	{
		super(source, proto, roles);
	}

	@Override
	public GDo reconstruct(org.scribble.ast.Do<Global> source,
			GProtocolName proto, List<Role> roles)
	{
		return new GDo(source, proto, roles);
	}

	@Override
	public GDo substitute(Substitutions<Role> subs)
	{
		return (GDo) super.substitute(subs);
	}

	@Override
	public GType getInlined(STypeInliner i)//, Deque<SubprotoSig> stack)
	{
		GProtocolName fullname = this.proto;
		SubprotoSig sig = new SubprotoSig(fullname, this.roles, 
				Collections.emptyList());  // FIXME
		if (i.hasSig(sig))
		{
			RecVar rv = i.makeRecVar(sig);
			return new GContinue(getSource(), rv);
		}
		i.pushSig(sig);
		GProtocol g = i.job.getJobContext().getIntermediate(fullname);
		Substitutions<Role> subs = 
				new Substitutions<>(g.roles, i.peek().roles);  // FIXME: args
		GSeq inlined = g.def.substitute(subs).getInlined(i);//, stack);  
				// i.e. returning a GSeq -- rely on parent GSeq to inline
		i.popSig();
		return inlined;
	}

	@Override
	public LType project(Role self)
	{
		// TODO: consider role fixing and do pruning
		throw new RuntimeException("TODO: " + this);
	}

	@Override
	public Set<Role> checkRoleEnabling(Set<Role> enabled) throws ScribbleException
	{
		throw new RuntimeException("Unsupported for Do: " + this);
	}

	@Override
	public Map<Role, Role> checkExtChoiceConsistency(Map<Role, Role> enablers)
			throws ScribbleException
	{
		throw new RuntimeException("Unsupported for Do: " + this);
	}

	@Override
	public org.scribble.ast.global.GDo getSource()
	{
		return (org.scribble.ast.global.GDo) super.getSource();
	}

	@Override
	public int hashCode()
	{
		int hash = 1303;
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
		if (!(o instanceof GDo))
		{
			return false;
		}
		return super.equals(o);  // Does canEquals
	}

	@Override
	public boolean canEquals(Object o)
	{
		return o instanceof GDo;
	}
}

