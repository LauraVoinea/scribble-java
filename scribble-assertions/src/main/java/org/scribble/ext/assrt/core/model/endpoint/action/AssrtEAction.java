package org.scribble.ext.assrt.core.model.endpoint.action;

import java.util.List;
import java.util.stream.Collectors;

import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtAnnotDataName;

public interface AssrtEAction  // TODO: should be extends EAction ?
{

	AssrtBFormula getAssertion();

	default String assertionToString()
	{
		AssrtBFormula ass = getAssertion();
		return "{" + ass.toString() + "}";
	}

	//AssrtDataTypeVar DUMMY_VAR = new AssrtDataTypeVar("_dum0");  // for statevars -- cf. actionvars, AssrtCoreGProtocolTranslator::makeFreshDataTypeVar starts from 1

	/*AssrtDataTypeVar getAnnotVar();
	AssrtArithFormula getArithExpr();*/
	List<AssrtAFormula> getStateExprs();  // Any edge may be a continue-edge with state exprs
			// Cf. AssrtStateVarArgAnnotNode::getAnnotExprs

	//LinkedHashMap<AssrtIntVar, AssrtAFormula> getPhantoms();
	List<AssrtAnnotDataName> getPhantoms();
	AssrtBFormula getPhantomAssertion();

	default String stateExprsToString()
	{
		List<AssrtAFormula> aforms = getStateExprs();
		return "<" + aforms.stream().map(Object::toString)
						.collect(Collectors.joining(", ")) + ">";
	}

	// TODO: take Map<AssrtIntVar, String> env
	default String phantomsToString()
	{
		List<AssrtAnnotDataName> phantom = getPhantoms();
		return "[" + phantom.stream().map(Object::toString)
				.collect(Collectors.joining(", ")) + "]";
	}
	
	default String phantomAssertionToString()
	{
		return "{" + getPhantomAssertion() + "}";
	}
}

