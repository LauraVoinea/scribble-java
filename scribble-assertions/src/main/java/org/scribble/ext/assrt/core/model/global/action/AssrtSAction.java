package org.scribble.ext.assrt.core.model.global.action;

import java.util.List;
import java.util.stream.Collectors;

import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;

// The _additional_ stuff an SSend/Recv needs to be an AssrtSSend/AssrtSRecv
public interface AssrtSAction
{
	AssrtBFormula getAssertion();

	default String assertionToString()
	{
		AssrtBFormula ass = getAssertion();
		return "{" + ass.toString() + "}";
	}

	List<AssrtAFormula> getStateExprs();  // Cf. AssrtCoreEAction
	
	default String stateExprsToString()
	{
		List<AssrtAFormula> exprs = getStateExprs();
		return
			"<" + exprs.stream().map(Object::toString)
						.collect(Collectors.joining(", ")) + ">";
	}
}
