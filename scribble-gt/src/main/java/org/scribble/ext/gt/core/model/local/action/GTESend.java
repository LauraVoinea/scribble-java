package org.scribble.ext.gt.core.model.local.action;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

public class GTESend<A extends ActionKind> extends ESend<A> implements GTEAction {

    public GTESend(int id, ModelFactory ef, Role peer, MsgId<?> mid, Payload pay) {
        super(id, ef, peer, mid, pay);
    }
}
