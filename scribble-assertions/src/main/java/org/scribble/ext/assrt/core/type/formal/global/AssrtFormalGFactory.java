package org.scribble.ext.assrt.core.type.formal.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLBranch;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLEnd;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLSelect;
import org.scribble.ext.assrt.core.type.formal.local.AssrtFormalLocal;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLTransfer;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;

public class AssrtFormalGFactory
{

	public static final AssrtFormalGFactory factory = new AssrtFormalGFactory();

	public AssrtFormalGFactory() {

	}

	public AssrtFormalGChoice branch(Role sender, Role receiver,
									 LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalGlobal>> cases) {
		return new AssrtFormalGChoice(sender, receiver, cases);
	}

	public AssrtFormalGEnd end() {
		return AssrtFormalGEnd.END;
	}

}
