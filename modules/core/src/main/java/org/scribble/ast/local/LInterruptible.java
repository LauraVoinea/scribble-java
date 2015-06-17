package org.scribble.ast.local;

import java.util.List;

import org.scribble.ast.Interruptible;
import org.scribble.ast.ScribNodeBase;
import org.scribble.ast.ProtocolBlock;
import org.scribble.ast.name.simple.ScopeNode;
import org.scribble.sesstype.kind.Local;
import org.scribble.sesstype.name.SimpleName;

//public class LocalInterruptible extends Interruptible<LocalProtocolBlock, LocalInterrupt> implements LocalInteractionNode
public class LInterruptible extends Interruptible<Local> implements LInteractionNode
{
	//protected LocalInterruptible(ScopeNode scope, LocalProtocolBlock block, List<LocalInterrupt> interrs)
	protected LInterruptible(ScopeNode scope, ProtocolBlock<Local> block, List<LInterrupt> interrs)
	{
		super(scope, block, interrs);
		// TODO Auto-generated constructor stub
	}

	@Override
	public boolean isEmptyScope()
	{
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public SimpleName getScopeElement()
	{
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	protected ScribNodeBase copy()
	{
		// TODO Auto-generated method stub
		return null;
	}

	/*public LocalInterruptible(CommonTree ct, ScopeNode scope, LocalProtocolBlock block, LocalThrows thro, List<LocalCatches> cats)
	{
		this(ct, scope, block, compileInterrupts(thro, cats), null, null);
	}

	/*public LocalInterruptible(CommonTree ct, ScopeNode scope, LocalProtocolBlock block, List<LocalInterrupt> interrs, CompoundInteractionNodeContext icontext)
	{
		super(ct, scope, block, interrs, icontext);
	}* /

	protected LocalInterruptible(CommonTree ct, ScopeNode scope, LocalProtocolBlock block, List<LocalInterrupt> interrs, CompoundInteractionNodeContext icontext, Env env)
	{
		super(ct, scope, block, interrs, icontext, env);
	}
	
	private static List<LocalInterrupt> compileInterrupts(LocalThrows thro, List<LocalCatches> cats)
	{
		List<LocalInterrupt> interrs = new LinkedList<>();
		if (thro != null)
		{
			interrs.add(thro);
		}
		interrs.addAll(cats);
		return interrs;
	}

	@Override
	protected LocalInterruptible reconstruct(CommonTree ct, ScopeNode scope, LocalProtocolBlock block, List<LocalInterrupt> interrs, CompoundInteractionNodeContext icontext, Env env)
	{
		return new LocalInterruptible(ct, scope, block, interrs, icontext, env);
	}
	
	@Override
	public LocalInterruptible leaveGraphBuilding(GraphBuilder builder)
	{
		throw new RuntimeException("TODO: ");
	}

	/*@Override
	public LocalInterruptible leaveContextBuilding(Node parent, NodeContextBuilder builder) throws ScribbleException
	{
		Interruptible<LocalProtocolBlock, LocalInterrupt> intt = super.leaveContextBuilding(parent, builder);
		return new LocalInterruptible(intt.ct, intt.scope, intt.block, interrs, (CompoundInteractionNodeContext) intt.getContext());
	}

	@Override
	public LocalInterruptible leaveWFChoiceCheck(WellFormedChoiceChecker checker) throws ScribbleException
	{
		Interruptible<LocalProtocolBlock, LocalInterrupt> intt = super.leaveWFChoiceCheck(checker);
		return new LocalInterruptible(intt.ct, intt.scope, intt.block, intt.interrs, (CompoundInteractionNodeContext) intt.getContext(), intt.getEnv());
	}

	@Override
	public LocalInterruptible visitChildren(NodeVisitor nv) throws ScribbleException
	{
		Interruptible<LocalProtocolBlock, LocalInterrupt> intt = super.visitChildren(nv);
		return new LocalInterruptible(intt.ct, intt.scope, intt.block, intt.interrs, intt.getContext(), intt.getEnv());
	}*/
}
