package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formal.local.action.*;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.Collections;
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

	public AssrtFormalLBranch branch(Role sender, LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		return new AssrtFormalLBranch(sender, cases);
	}

	public AssrtFormalLSelect select(Role receiver, LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		return new AssrtFormalLSelect(receiver, cases);
	}

	public AssrtFormalLRec rec(RecVar recvar, AssrtFormalLType body,
							   LinkedHashMap<AssrtVar, Triple<Multiplicity, DataName, AssrtAFormula>> svars,
							   AssrtBFormula ass) {
		return new AssrtFormalLRec(recvar, body, svars, ass);
	}

	public AssrtFormalLRecVar recvar(RecVar recvar,
			 LinkedHashMap<AssrtVar, Pair<Multiplicity, AssrtAFormula>> svars,
			 AssrtBFormula ass) {
		return new AssrtFormalLRecVar(recvar, svars, ass);
	}

	public AssrtFormalLSilent silent(LinkedHashMap<Op, Pair<AssrtMsg, AssrtFormalLType>> cases) {
		return new AssrtFormalLSilent(cases);
	}

	public AssrtFormalLEnd end() {
		return AssrtFormalLEnd.END;
	}

	/* Actions -- hardcoded to single refinement vars */

	public AssrtFormalLSend send(Role receiver, AssrtMsg m) {
		return send(receiver, m, Collections.emptyList());
	}

	public AssrtFormalLSend send(Role receiver, AssrtMsg m, List<AssrtMsg> consumed) {
		return new AssrtFormalLSend(receiver, m, consumed);
	}

	public AssrtFormalLReceive receive(Role sender, AssrtMsg m) {
		return receive(sender, m, Collections.emptyList());
	}

	public AssrtFormalLReceive receive(Role sender, AssrtMsg m, List<AssrtMsg> consumed) {
		return new AssrtFormalLReceive(sender, m, consumed);
	}

	public AssrtFormalLEnter enter(RecVar recvar, AssrtVar svar, Multiplicity multip,
								   DataName data, AssrtAFormula init, AssrtBFormula assertion) {
		return enter(recvar, svar, multip, data, init, assertion, Collections.emptyList());
	}

	public AssrtFormalLEnter enter(RecVar recvar, AssrtVar svar, Multiplicity multip,
								   DataName data, AssrtAFormula init,
								   AssrtBFormula assertion, List<AssrtMsg> silents) {
		return new AssrtFormalLEnter(recvar, svar, multip, data, init, assertion, silents);
	}

	public AssrtFormalLContinue continu(RecVar recvar, AssrtVar svar, Multiplicity multip,
										AssrtAFormula init) {
		return continu(recvar, svar, multip, init, Collections.emptyList());
	}

	public AssrtFormalLContinue continu(RecVar recvar, AssrtVar svar, Multiplicity multip,
										AssrtAFormula init, List<AssrtMsg> silents) {
		return new AssrtFormalLContinue(recvar, svar, multip, init, silents);
	}

	public AssrtFormalLEpsilon epsilon(AssrtMsg m) {
		return new AssrtFormalLEpsilon(m);
	}

}
