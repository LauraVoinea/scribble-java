package org.scribble.ext.assrt.core.type.formal.local;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.type.kind.Local;
import org.scribble.core.type.kind.NonRoleParamKind;
import org.scribble.core.type.name.ProtoName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Arg;
import org.scribble.core.type.session.Msg;
import org.scribble.core.type.session.local.*;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLTransfer;
import org.scribble.ext.assrt.core.type.formula.AssrtAFormula;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.ext.assrt.core.type.session.local.*;

import java.util.LinkedHashMap;
import java.util.List;

public class AssrtLFormalFactory
{

	/* Actions */

	public AssrtLTransfer AssrtLTransfer(Role sender, Role receiver, AssrtMsg msg) {
		return new AssrtLTransfer(sender, receiver, msg);
	}



}
