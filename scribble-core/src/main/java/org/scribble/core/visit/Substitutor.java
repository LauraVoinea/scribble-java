package org.scribble.core.visit;

import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;

import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.kind.ProtocolKind;
import org.scribble.core.type.name.DataType;
import org.scribble.core.type.name.MemberName;
import org.scribble.core.type.name.MessageSigName;
import org.scribble.core.type.name.ProtocolName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.name.Substitutions;
import org.scribble.core.type.session.Arg;
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.DirectedInteraction;
import org.scribble.core.type.session.DisconnectAction;
import org.scribble.core.type.session.Do;
import org.scribble.core.type.session.Message;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.Seq;

public class Substitutor<K extends ProtocolKind, B extends Seq<K, B>>
		extends STypeVisitorNoThrow<K, B>
{
	private Substitutions subs;

	public Substitutor(List<Role> rold, List<Role> rnew,
			List<MemberName<? extends NonRoleParamKind>> aold,
			List<Arg<? extends NonRoleParamKind>> anew)
	{
		this.subs = new Substitutions(rold, rnew, aold, anew);
	}

	@Override
	public SType<K, B> visitChoice(Choice<K, B> n)
	{
		List<B> blocks = n.blocks.stream().map(x -> visitSeq(x))
				.collect(Collectors.toList());
		return n.reconstruct(n.getSource(), this.subs.subsRole(n.subj), blocks);
	}

	@Override
	public SType<K, B> visitDirectedInteraction(DirectedInteraction<K, B> n)
	{
		Message msg = n.msg;
		if (msg instanceof MemberName)
		{
			MemberName<?> name = (MemberName<?>) msg;
			if (this.subs.hasArg(name))
			{
				msg = (Message) this.subs.subsArg(name);
			}
		}
		return n.reconstruct(n.getSource(), msg, this.subs.subsRole(n.src),
				this.subs.subsRole(n.dst));
	}

	@Override
	public SType<K, B> visitDisconnect(DisconnectAction<K, B> n)
	{
		return n.reconstruct(n.getSource(), this.subs.subsRole(n.left),
				this.subs.subsRole(n.right));
	}

	@Override
	public <N extends ProtocolName<K>> SType<K, B> visitDo(Do<K, B, N> n)
	{
		List<Role> roles = n.roles.stream().map(x -> this.subs.subsRole(x))
				.collect(Collectors.toList());
		List<Arg<? extends NonRoleParamKind>> args = new LinkedList<>();
		for (Arg<? extends NonRoleParamKind> a : n.args) 
		{
			if (a instanceof MemberName<?> && this.subs.hasArg((MemberName<?>) a))
			{
				if (a instanceof DataType)
				{
					a = this.subs.subsArg((DataType) a);
				}
				else if (a instanceof MessageSigName)
				{
					a = this.subs.subsArg((MessageSigName) a);
				}
			}
			args.add(a);
		}
		return n.reconstruct(n.getSource(), n.proto, roles, args);
	}
}
