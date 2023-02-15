package org.scribble.ext.assrt.core.type.formal.local.action;

import org.scribble.ext.assrt.core.type.session.AssrtMsg;

import java.util.List;

public interface AssrtFormalLDerivedAction extends AssrtFormalLAction {

    List<AssrtMsg> getSilent();
    AssrtFormalLDerivedAction prependSilent(AssrtMsg m);
    AssrtFormalLAction drop();
}
