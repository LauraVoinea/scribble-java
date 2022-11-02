package org.scribble.ext.assrt.core.type.formal;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.global.AssrtFormalGFactory;
import org.scribble.ext.assrt.core.type.formal.local.*;
import org.scribble.ext.assrt.core.type.formal.local.action.AssrtLTransfer;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;

public class AssrtFormalFactory
{
	public static final AssrtFormalFactory factory = new AssrtFormalFactory();

	public final AssrtFormalGFactory global = AssrtFormalGFactory.factory;
	public final AssrtFormalLFactory local = AssrtFormalLFactory.factory;

	protected AssrtFormalFactory() {

	}
}
