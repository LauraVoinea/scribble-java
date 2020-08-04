package org.scribble.ext.assrt.core.type.formula.dep;

import java.util.List;

import org.scribble.ext.assrt.core.type.name.AssrtSort;

@Deprecated
public interface AssrtUnintFunFormula
{
	List<AssrtSort> getParamSorts();  // FIXME: maybe should not be here -- formula should be arg exprs only (sorts given by member decl)
	AssrtSort getReturnSort();
}
