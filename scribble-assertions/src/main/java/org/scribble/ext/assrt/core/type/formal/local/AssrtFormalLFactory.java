package org.scribble.ext.assrt.core.type.formal.local;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Arg;
import org.scribble.core.type.session.Msg;
import org.scribble.core.type.session.local.*;
import org.scribble.ext.assrt.core.type.formal.global.AssrtFormalGFactory;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLEpsilon;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLReceive;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLSend;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLTransfer;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.*;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.List;

public class AssrtFormalLFactory
{

	public static final AssrtFormalLFactory factory = new AssrtFormalLFactory();

	public AssrtFormalLFactory() {

	}

	public AssrtLTransfer AssrtLTransfer(Role sender, Role receiver, AssrtMsg msg) {
		return new AssrtLTransfer(sender, receiver, msg);
	}

	public AssrtFormalLBranch branch(Role sender, LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases) {
		return new AssrtFormalLBranch(sender, cases);
	}

	public AssrtFormalLSelect select(Role receiver, LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases) {
		return new AssrtFormalLSelect(receiver, cases);
	}

	public AssrtFormalLSilent silent(LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLocal>> cases) {
		return new AssrtFormalLSilent(cases);
	}

	public AssrtFormalLEnd end() {
		return AssrtFormalLEnd.END;
	}

	/* Actions */

	public AssrtLSend send(Role receiver, AssrtMsg m) {
		return new AssrtLSend(receiver, m);
	}

	public AssrtLSend send(Role receiver, AssrtMsg m, List<AssrtMsg> consumed) {
		return new AssrtLSend(receiver, m, consumed);
	}

	public AssrtLReceive receive(Role sender, AssrtMsg m) {
		return new AssrtLReceive(sender, m);
	}

	public AssrtLReceive receive(Role sender, AssrtMsg m, List<AssrtMsg> consumed) {
		return new AssrtLReceive(sender, m, consumed);
	}

	public AssrtLEpsilon epsilon(AssrtMsg m) {
		return new AssrtLEpsilon(m);
	}

}
