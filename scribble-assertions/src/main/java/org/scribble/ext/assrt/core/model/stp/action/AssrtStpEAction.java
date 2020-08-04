package org.scribble.ext.assrt.core.model.stp.action;

import java.util.Map;

import org.scribble.ext.assrt.core.model.endpoint.action.AssrtCoreEAction;
import org.scribble.ext.assrt.core.type.formula.AssrtVarFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtSmtFormula;

public interface AssrtStpEAction extends AssrtCoreEAction
{
	Map<AssrtVarFormula, AssrtSmtFormula<?>> getSigma();
}
