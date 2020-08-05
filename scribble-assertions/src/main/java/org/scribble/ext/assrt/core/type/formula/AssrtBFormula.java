package org.scribble.ext.assrt.core.type.formula;

// Binary boolean -- top-level formula of assertions
// N.B. equals/hashCode is only for "syntactic" comparison
public abstract class AssrtBFormula extends AssrtSmtFormula
{
	public AssrtBFormula()
	{

	}
	
	@Override
	public abstract AssrtBFormula squash();

	@Override
	public abstract AssrtBFormula subs(AssrtAVarFormula old, AssrtAVarFormula neu);
}
