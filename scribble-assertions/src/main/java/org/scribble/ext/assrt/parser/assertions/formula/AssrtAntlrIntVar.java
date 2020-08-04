package org.scribble.ext.assrt.parser.assertions.formula;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.ext.assrt.core.type.formula.AssrtFormulaFactory;
import org.scribble.ext.assrt.core.type.formula.AssrtVarFormula;
import org.scribble.ext.assrt.parser.assertions.AssrtAntlrToFormulaParser;

public class AssrtAntlrIntVar
{
	public static AssrtVarFormula parseIntVar(AssrtAntlrToFormulaParser parser, CommonTree root)
	{
		return AssrtFormulaFactory.AssrtIntVar(root.getChild(0).getText());
	}
}
