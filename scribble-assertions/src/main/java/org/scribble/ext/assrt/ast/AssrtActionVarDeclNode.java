package org.scribble.ext.assrt.ast;

import org.scribble.ext.assrt.ast.name.simple.AssrtVarNameNode;

// Modifiers, e.g., "lin", will probably be added here
public interface AssrtActionVarDeclNode
{
	AssrtVarNameNode getAnnotVar();  // CHECKME: needed?
}
