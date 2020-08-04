package org.scribble.ext.assrt.core.type.formula.dep;

import java.util.List;

import org.scribble.ext.assrt.core.type.name.AssrtSort;
import org.sosy_lab.java_smt.api.Formula;

@Deprecated
public interface AssrtUnintFunFormula<F extends Formula> 
{
	List<AssrtSort> getParamSorts();  // FIXME: maybe should not be here -- formula should be arg exprs only (sorts given by member decl)
	AssrtSort getReturnSort();
}
