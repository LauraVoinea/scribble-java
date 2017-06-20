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
package org.scribble.ast.local;

import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.ast.AssertionNode;
import org.scribble.ast.AAstFactoryImpl;
import org.scribble.ast.Constants;
import org.scribble.ast.MessageNode;
import org.scribble.ast.ScribNodeBase;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.del.AScribDel;
import org.scribble.main.RuntimeScribbleException;
import org.scribble.main.ScribbleException;
import org.scribble.sesstype.Message;
import org.scribble.sesstype.kind.Local;
import org.scribble.sesstype.name.Role;
import org.scribble.util.ScribUtil;
import org.scribble.visit.context.ProjectedChoiceSubjectFixer;

public class ALSend extends ALMessageTransfer
		implements LSimpleInteractionNode  // Explicitly needed here for getKind
{
	public ALSend(CommonTree source, RoleNode src, MessageNode msg, List<RoleNode> dests, AssertionNode assertion)
	{
		super(source, src, msg, dests, assertion);
	}

	@Override
	protected ScribNodeBase copy()
	{
		return new ALSend(this.source, this.src, this.msg, getDestinations(), this.assertion);
	}
	
	@Override
	public ALSend clone()
	{
		RoleNode src = this.src.clone();
		MessageNode msg = this.msg.clone();
		List<RoleNode> dests = ScribUtil.cloneList(getDestinations());
		return AAstFactoryImpl.FACTORY.LSend(this.source, src, msg, dests, this.assertion);
	}

	@Override
	public ALSend reconstruct(RoleNode src, MessageNode msg, List<RoleNode> dests, AssertionNode assertion)
	{
		AScribDel del = del();
		ALSend ls = new ALSend(this.source, src, msg, dests, assertion);
		ls = (ALSend) ls.del(del);
		return ls;
	}

	// Could make a LMessageTransfer to factor this out with LReceive
	@Override
	public Role inferLocalChoiceSubject(ProjectedChoiceSubjectFixer fixer)
	{
		return this.src.toName();
	}

	// FIXME: shouldn't be needed, but here due to Eclipse bug https://bugs.eclipse.org/bugs/show_bug.cgi?id=436350
	@Override
	public Local getKind()
	{
		return LSimpleInteractionNode.super.getKind();
	}

	@Override
	public String toString()
	{
		return this.msg + " " + Constants.TO_KW + " "
					+ getDestinations().stream().map((dest) -> dest.toString()).collect(Collectors.joining(", ")) + ";";
	}

	@Override
	public LInteractionNode merge(LInteractionNode ln) throws ScribbleException
	{
		throw new RuntimeScribbleException("Invalid merge on LSend: " + this);
	}

	@Override
	public boolean canMerge(LInteractionNode ln)
	{
		return false;
	}

	@Override
	public Set<Message> getEnabling()
	{
		return Collections.emptySet();
	}
}
