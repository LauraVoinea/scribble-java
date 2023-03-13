package org.scribble.ext.gt.core.model.local.action;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

@Deprecated
public abstract class GTEActionBase<A extends ActionKind> extends EAction<A> {

    protected GTEActionBase(int id, ModelFactory mf, Role peer, MsgId<?> mid, Payload pay) {
        super(id, mf, peer, mid, pay);
    }
}
