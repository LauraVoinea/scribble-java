package org.scribble.core.visit;

import java.util.LinkedList;
import java.util.List;

import org.scribble.core.type.kind.ProtocolKind;
import org.scribble.core.type.name.ProtocolName;
import org.scribble.core.type.session.Choice;
import org.scribble.core.type.session.Continue;
import org.scribble.core.type.session.DirectedInteraction;
import org.scribble.core.type.session.DisconnectAction;
import org.scribble.core.type.session.Do;
import org.scribble.core.type.session.Recursion;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.Seq;
import org.scribble.util.ScribException;

public abstract class STypeVisitor<K extends ProtocolKind, B extends Seq<K, B>>
{
	// SType return for extensibility/flexibility
	public SType<K, B> visitContinue(Continue<K, B> n) throws ScribException
	{
		//return n.reconstruct(n.getSource(), n.recvar);
		return n;
	}

	public SType<K, B> visitChoice(Choice<K, B> n) throws ScribException
	{
		List<B> blocks = new LinkedList<>();
		for (B b : n.blocks)
		{
			blocks.add(b.visitWith(this));
		}
		return n.reconstruct(n.getSource(), n.subj, blocks);
	}

	public SType<K, B> visitDirectedInteraction(DirectedInteraction<K, B> n)
			throws ScribException
	{
		//return n.reconstruct(n.getSource(), n.msg, n.src, n.dst);
		return n;
	}

	public SType<K, B> visitDisconnect(DisconnectAction<K, B> n)
			throws ScribException
	{
		//return n.reconstruct(n.getSource(), n.left, n.right);
		return n;
	}

	public <N extends ProtocolName<K>> SType<K, B> visitDo(Do<K, B, N> n)
			throws ScribException
	{
		//return n.reconstruct(n.getSource(), n.proto, n.roles, n.args);
		return n;
	}

	public SType<K, B> visitRecursion(Recursion<K, B> n) throws ScribException
	{
		B body = n.body.visitWith(this);
		return n.reconstruct(n.getSource(), n.recvar, body);
	}

	// "Hardcoded" to B (cf. Seq, or SType return) -- this visitor pattern depends on B for Choice/Recursion/etc reconstruction
	// This means a Visitor that needs to restructure a Seq should handle this within visitSeq
	// E.g., Seq "injection" by inlining and unfolding
	// For this purpose, visited children passed "directly" instead of via a reconstruction (cf. above methods)
	public B visitSeq(B n) throws ScribException
	{
		List<SType<K, B>> elems = new LinkedList<>();
		for (SType<K, B> e : n.elems)
		{
			elems.add(e.visitWith(this));
		}
		return n.reconstruct(n.getSource(), elems);
	}
}




















/*@FunctionalInterface
interface STypeVisitorFunction<K extends ProtocolKind, B extends Seq<K, B>, R extends Stream<?>>
{
	R f(SType<K, B> n, STypeVisitor<K, B> v);
}*/
