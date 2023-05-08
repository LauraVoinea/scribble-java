package org.scribble.ext.gt.core.model.global;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.global.SModelFactoryImpl;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.model.global.actions.SSend;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.global.action.GTSRecv;
import org.scribble.ext.gt.core.model.global.action.GTSSend;

public class GTSModelFactoryImpl extends SModelFactoryImpl implements GTSModelFactory {

    public GTSModelFactoryImpl(ModelFactory mf) {
        super(mf);
    }

    public <A extends ActionKind> GTSSend<A> GTSSend(Role subj, Role obj, MsgId<?> mid, Payload pay, int c, int n) {
        return new GTSSend<>(subj, obj, mid, pay, c, n);
    }

    public <A extends ActionKind> GTSRecv<A> GTSRecv(Role subj, Role obj, MsgId<?> mid, Payload pay, int c, int n) {
        return new GTSRecv<>(subj, obj, mid, pay, c, n);
    }

    @Override
    public GTSNewTimeout SNewTimeout(int c, int n) {
        return new GTSNewTimeout(c, n);
    }
}
