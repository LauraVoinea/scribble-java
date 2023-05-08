package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.*;
import org.scribble.core.model.endpoint.EModelFactoryImpl;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.model.global.SModelFactoryImpl;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.global.action.GTSRecv;
import org.scribble.ext.gt.core.model.global.action.GTSSend;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.core.model.local.action.GTERecv;
import org.scribble.ext.gt.core.model.local.action.GTESend;

public class GTEModelFactoryImpl extends EModelFactoryImpl implements GTEModelFactory {

    public GTEModelFactoryImpl(ModelFactory mf) {
        super(mf);
    }

    @Override
    public GTESend<DynamicActionKind> DynamicGTESend(Role peer, MsgId<?> mid, Payload pay, int c, int n) {
        return new GTESend<>(MActionBase.nextCount(), this.mf, peer, mid, pay, c, n);
    }

    @Override
    public GTERecv<DynamicActionKind> DynamicGTERecv(Role peer, MsgId<?> mid, Payload pay, int c, int n) {
        return new GTERecv<>(MActionBase.nextCount(), this.mf, peer, mid, pay, c, n);
    }

    @Override
    public GTENewTimeout<DynamicActionKind> DynamicGTENewTimeout(int c, int n) {
        return new GTENewTimeout<>(MActionBase.nextCount(), this.mf, c, n);
    }
}
