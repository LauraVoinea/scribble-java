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
package org.scribble.ast;

import org.antlr.runtime.Token;
import org.scribble.ast.name.PayloadElemNameNode;
import org.scribble.core.type.kind.PayloadTypeKind;
import org.scribble.core.type.name.PayloadElemType;
import org.scribble.del.ScribDel;
import org.scribble.util.ScribException;
import org.scribble.visit.AstVisitor;


// TODO: make abstract, and make a DataTypeElem subclass with DataTypeKind parameter instance (alongside LDelegationElem)

// FIXME: refactor: generic typing issues, no concrete name<K> field any more
// Cf. DoArg, wrapper for a (unary) name node of potentially unknown kind (needs disamb)
// PayloadTypeKind is DataType or Local, but Local has its own special subclass (and protocol params not allowed), so this should implicitly be for DataType only
// AST hierarchy requires unary and delegation (binary pair) payloads to be structurally distinguished
//public class DataTypeElem extends PayloadElem<DataTypeKind>
public class UnaryPayloadElem<K extends PayloadTypeKind> extends ScribNodeBase
		implements PayloadElem<K>// extends PayloadElem
{
	// cf. Scribble.g
	public static final int NAME_CHILD_INDEX = 0;
	
	// ScribTreeAdaptor#create constructor
	public UnaryPayloadElem(Token t)
	{
		super(t);
	}

	// Tree#dupNode constructor
	public UnaryPayloadElem(UnaryPayloadElem<K> node)
	{
		super(node);
	}
	
	public PayloadElemNameNode<K> getNameChild()
	{
		// FIXME
		PayloadElemNameNode<K> name = (PayloadElemNameNode<K>) getChild(NAME_CHILD_INDEX);  
				// CHECKME: probably need to record an explicit kind token, for "cast checking"
				// Cannot use ScribNodeBase.visitChildWithCastCheck because this is not a ProtocolKindNode
		return name;
	}
	
	@Override
	public UnaryPayloadElem<K> dupNode()
	{
		return new UnaryPayloadElem<>(this);
	}

	public UnaryPayloadElem<K> reconstruct(PayloadElemNameNode<K> name)
	{
		UnaryPayloadElem<K> elem = dupNode();
		elem.addChild(name);
		ScribDel del = del();
		elem.setDel(del);  // No copy
		return elem;
	}

	@Override
	public UnaryPayloadElem<K> visitChildren(AstVisitor nv)
			throws ScribException
	{
		@SuppressWarnings("unchecked")
		PayloadElemNameNode<K> name = (PayloadElemNameNode<K>) visitChild(
				getNameChild(), nv);
				// CHECKME: probably need to record an explicit kind token, for "cast checking"
				// Cannot use ScribNodeBase.visitChildWithCastCheck because this is not a ProtocolKindNode
		return reconstruct(name);
	}

	@Override
	public PayloadElemType<K> toPayloadType()
	{
		return getNameChild().toPayloadType();
	}
	
	@Override
	public String toString()
	{
		return getNameChild().toString();
	}
}

