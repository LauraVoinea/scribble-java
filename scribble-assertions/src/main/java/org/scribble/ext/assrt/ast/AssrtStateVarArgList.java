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
package org.scribble.ext.assrt.ast;

import java.util.List;
import java.util.stream.Collectors;

import org.antlr.runtime.Token;
import org.scribble.ast.ScribNodeBase;
import org.scribble.del.DelFactory;
import org.scribble.ext.assrt.del.AssrtDelFactory;
import org.scribble.util.ScribException;
import org.scribble.visit.AstVisitor;


// Duplicated from AssrtStateVarDeclList
public class AssrtStateVarArgList extends ScribNodeBase//ParamDeclList<AssrtIntVarKind>
{
	// ScribTreeAdaptor#create constructor
	public AssrtStateVarArgList(Token t)
	{
		super(t);
	}

	// Tree#dupNode constructor
	public AssrtStateVarArgList(AssrtStateVarArgList node)
	{
		super(node);
	}
	
	public List<AssrtAExprNode> getAnnotExprChildren()
	{
		return ((List<?>) getChildren()).stream().map(x -> (AssrtAExprNode) x)
				.collect(Collectors.toList());
	}

	// "add", not "set"
	public void addScribChildren(List<AssrtAExprNode> sexprs)
	{
		// Cf. above getters and Scribble.g children order
		addChildren(sexprs);
	}
	
	@Override
	public AssrtStateVarArgList dupNode()
	{
		return new AssrtStateVarArgList(this);
	}
	
	@Override
	public void decorateDel(DelFactory df)
	{
		((AssrtDelFactory) df).AssrtStateVarArgList(this);
		// Created by AssertionsTreeAdaptor (no del dec), not AssrtScribTreeAdaptor, but del decoration done again(?) in Job.runPasses
	}

	public AssrtStateVarArgList reconstruct(List<AssrtAExprNode> sexprs)
	{
		AssrtStateVarArgList dup = dupNode();
		dup.addScribChildren(sexprs);
		dup.setDel(del());  // No copy
		return dup;
	}

	@Override
	public AssrtStateVarArgList visitChildren(AstVisitor v) throws ScribException
	{
		List<AssrtAExprNode> sexprs = visitChildListWithClassEqualityCheck(this,
				getAnnotExprChildren(), v);  // Supports empty list
		return reconstruct(sexprs);
	}

	@Override
	public String toString()
	{
		return "@'[" + getChildren().stream().map(Object::toString)
				.collect(Collectors.joining(", ")) + "]'";
	}
}
