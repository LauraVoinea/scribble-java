package org.scribble.ext.assrt.core.type.formula;

import java.util.Map;

import org.scribble.core.type.name.DataName;
import org.scribble.ext.assrt.core.type.name.AssrtVar;

public abstract class AssrtAFormula extends AssrtSmtFormula
{
	@Override
	public abstract AssrtAFormula squash();

	// Factor out with AssrtBFormula?
	@Override
	public abstract AssrtAFormula subs(AssrtAVarFormula old,
			AssrtAVarFormula neu);

	// TODO: factor out a SortEnv type
	public abstract DataName getSort(Map<AssrtVar, DataName> env);

	// i.e., does not contain any AssrtIntVarFormula -- n.b., includes but not equal to literals
	public abstract boolean isConstant();
}
