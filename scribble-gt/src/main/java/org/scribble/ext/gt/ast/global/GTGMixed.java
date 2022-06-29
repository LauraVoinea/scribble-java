/*
 * Copyright 2008 The Scribble Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scribble.ext.gt.ast.global;

import org.antlr.runtime.Token;
import org.scribble.ast.Choice;
import org.scribble.ast.Recursion;
import org.scribble.ast.ScribNode;
import org.scribble.ast.global.GCompoundSessionNode;
import org.scribble.ast.global.GProtoBlock;
import org.scribble.core.type.kind.Global;
import org.scribble.del.DelFactory;
import org.scribble.ext.gt.ast.GTMixed;
import org.scribble.ext.gt.del.GTDelFactory;

import java.util.List;
import java.util.stream.Collectors;

public class GTGMixed extends GTMixed<Global> implements GCompoundSessionNode
{
	// ScribTreeAdaptor#create constructor
	public GTGMixed(Token t)
	{
		super(t);
	}

	@Override
	public GProtoBlock getLeftBlockChild() {
		return (GProtoBlock) getChild(GTMixed.LEFT_BLOCK_CHILD_INDEX);
	}

	@Override
	public GProtoBlock getRightBlockChild() {
		return (GProtoBlock) getChild(GTMixed.RIGHT_BLOCK_CHILD_INDEX);
	}

	// Tree#dupNode constructor
	protected GTGMixed(GTGMixed node)
	{
		super(node);
	}
	
	@Override
	public List<GProtoBlock> getBlockChildren()
	{
		List<? extends ScribNode> cs = getChildren();
		return cs.subList(Choice.BLOCK_CHILDREN_START_INDEX, cs.size()).stream()
				.map(x -> (GProtoBlock) x).collect(Collectors.toList());
	}
	
	@Override
	public GTGMixed dupNode()
	{
		return new GTGMixed(this);
	}
	
	@Override
	public void decorateDel(DelFactory df)
	{
		((GTDelFactory) df).GTGMixed(this);
	}
}
