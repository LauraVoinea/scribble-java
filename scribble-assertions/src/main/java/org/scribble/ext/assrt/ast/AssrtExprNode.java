package org.scribble.ext.assrt.ast;

import org.scribble.ext.assrt.core.type.formula.AssrtSmtFormula;

// TODO: rename AnnotExprNode
public interface AssrtExprNode
{
	AssrtSmtFormula getFormula();
}
