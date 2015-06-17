package org.scribble.ast.local;

import java.util.List;

import org.scribble.ast.Constants;
import org.scribble.ast.MessageNode;
import org.scribble.ast.MessageTransfer;
import org.scribble.ast.ScribNodeBase;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.del.ScribDel;
import org.scribble.sesstype.kind.Local;

public class LReceive extends MessageTransfer<Local> implements LSimpleInteractionNode
{
	// HACK: will make dummy dest Nodes, would be better to just be Roles
	public LReceive(RoleNode src, MessageNode msg, List<RoleNode> dests)
	{
		super(src, msg, dests);
	}

	@Override
	protected LReceive reconstruct(RoleNode src, MessageNode msg, List<RoleNode> dests)
	{
		ScribDel del = del();
		LReceive lr = new LReceive(src, msg, dests);
		lr = (LReceive) lr.del(del);
		return lr;
	}
	
	/*@Override
	public LocalReceive leaveGraphBuilding(GraphBuilder builder)
	{
		ProtocolState entry = builder.getEntry();
		ProtocolState exit = builder.getExit();
		// Projection means single self dest role (asymmetric with multicast send)

		// FIXME:
		Message msg = LocalSend.getGraphMessage(builder, this.msg);

		UnscopedMulticastSignature msig = new UnscopedMulticastSignature(this.src.toName(), getDestinationRoles(), msg);
		entry.edges.put(msig, exit);
		return this;
	}*/

	/*@Override
	public LocalReceive visitChildren(NodeVisitor nv) throws ScribbleException
	{
		MessageTransfer mt = super.visitChildren(nv);
		return new LocalReceive(mt.ct, mt.src, mt.msg, mt.dests);
	}*/

	@Override
	public String toString()
	{
		return this.msg + " " + Constants.FROM_KW + " " + this.src + ";";
	}

	@Override
	protected ScribNodeBase copy()
	{
		return new LReceive(this.src, this.msg, this.dests);
	}
	
	/*// Make a LocalCommunication base class
	@Override
	public void toGraph(GraphBuilder gb)
	{
		LocalSend.toFSM(gb, this);
	}*/
}
