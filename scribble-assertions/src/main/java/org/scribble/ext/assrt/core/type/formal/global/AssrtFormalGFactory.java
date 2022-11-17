package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Set;

public class AssrtFormalGFactory
{

	public static final AssrtFormalGFactory factory = new AssrtFormalGFactory();

	public AssrtFormalGFactory() {

	}

	public AssrtFormalGChoice branch(Role sender, Role receiver,
									 LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGType>> cases) {
		return new AssrtFormalGChoice(sender, receiver, cases);
	}

	public AssrtFormalGRec rec(RecVar recvar, AssrtFormalGType body,
							   LinkedHashMap<AssrtVar, Triple<Set<Role>, DataName, AssrtAFormula>> svars,
							   AssrtBFormula ass) {
		return new AssrtFormalGRec(recvar, body, svars, ass);
	}

	public AssrtFormalGRecVar recvar(RecVar recvar,
									 LinkedHashMap<AssrtVar, AssrtAFormula> svars,
									 AssrtBFormula ass) {
		return new AssrtFormalGRecVar(recvar, svars, ass);
	}

	public AssrtFormalGEnd end() {
		return AssrtFormalGEnd.END;
	}

}
