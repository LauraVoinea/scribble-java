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
package org.scribble.ext.gt.ast;

import org.antlr.runtime.Token;
import org.scribble.ast.CompoundInteraction;
import org.scribble.ast.ProtoBlock;
import org.scribble.ast.RoleArgList;
import org.scribble.ast.name.simple.RoleNode;
import org.scribble.core.type.kind.ProtoKind;
import org.scribble.util.Constants;
import org.scribble.util.ScribException;
import org.scribble.visit.AstVisitor;

// Duplicated from ast.Choice
public abstract class GTMixed<K extends ProtoKind>
		extends CompoundInteraction<K>
{
	public static final int LEFT_BLOCK_CHILD_INDEX = 0;
	public static final int LEFT_ROLEARGS_CHILD_INDEX = 1;
	public static final int SECONDARY_CHILD_INDEX = 2;
	public static final int PRIMARY_CHILD_INDEX = 3;
	public static final int RIGHT_ROLEARGS_CHILD_INDEX = 4;
	public static final int RIGHT_BLOCK_CHILD_INDEX = 5;

	// ScribTreeAdaptor#create constructor
	public GTMixed(Token t)
	{
		super(t);
	}

	// Tree#dupNode constructor
	protected GTMixed(GTMixed<K> node)
	{
		super(node);
	}
	
	public abstract ProtoBlock<K> getLeftBlockChild();
	public abstract ProtoBlock<K> getRightBlockChild();

	public RoleNode getSecondaryChild() {
		return (RoleNode) getChild(SECONDARY_CHILD_INDEX);
	}

	public RoleNode getPrimaryChild() {
		return (RoleNode) getChild(PRIMARY_CHILD_INDEX);
	}

	public RoleArgList getLeftRoleListChild()  // cf. ast.Do
	{
		return (RoleArgList) getChild(LEFT_ROLEARGS_CHILD_INDEX);
	}

	public RoleArgList getRightRoleListChild()  // cf. ast.Do
	{
		return (RoleArgList) getChild(RIGHT_ROLEARGS_CHILD_INDEX);
	}

	// "add", not "set"
	public void addScribChildren(
			ProtoBlock<K> left, RoleArgList leftCommitted,
			RoleNode sec, RoleNode pri, RoleArgList rightCommitted,
			ProtoBlock<K> right)
	{
		// Cf. above getters and Scribble.g children order
		addChild(left);
		addChild(leftCommitted);
		addChild(sec);
		addChild(pri);
		addChild(rightCommitted);
		addChild(right);
	}
	
	@Override
	public abstract GTMixed<K> dupNode();
	
	public GTMixed<K> reconstruct(
			ProtoBlock<K> left, RoleArgList leftCommitted,
			RoleNode sec, RoleNode pri,
			RoleArgList rightCommitted, ProtoBlock<K> right)
	{
		GTMixed<K> dup = dupNode();
		dup.addScribChildren(left, leftCommitted, sec, pri, rightCommitted, right);
		dup.setDel(del());  // No copy
		return dup;
	}
	
	@Override
	public GTMixed<K> visitChildren(AstVisitor nv) throws ScribException
	{
		ProtoBlock<K> left =
				visitChildWithClassEqualityCheck(this, getLeftBlockChild(), nv);
		RoleArgList leftCommitted =
				(RoleArgList) visitChild(getLeftRoleListChild(), nv);
		RoleNode obs =
				(RoleNode) visitChild(getSecondaryChild(), nv);
		RoleNode tim =
				(RoleNode) visitChild(getPrimaryChild(), nv);
		RoleArgList rightCommitted =
				(RoleArgList) visitChild(getRightRoleListChild(), nv);
		ProtoBlock<K> right =
				visitChildWithClassEqualityCheck(this, getRightBlockChild(), nv);
		return reconstruct(left, leftCommitted, obs, tim, rightCommitted, right);
	}
	
	@Override
	public String toString()
	{
		return "mixed " + getLeftBlockChild() + " " + getLeftRoleListChild()
				+ " " + Constants.OR_KW + " "
				+ getSecondaryChild()  + "->" + getPrimaryChild() + " "
				+ getRightRoleListChild() + " " + getRightBlockChild();
	}
}
