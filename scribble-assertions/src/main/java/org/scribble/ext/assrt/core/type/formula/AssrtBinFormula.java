package org.scribble.ext.assrt.core.type.formula;

// N.B. F is kind of the children formulae (not the parent, this)
public interface AssrtBinFormula
{
	AssrtSmtFormula getLeft();

	AssrtSmtFormula getRight();
}
