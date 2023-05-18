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

    public static ModelFactory FACTORY;  // HACK refactor

    public GTEModelFactoryImpl(ModelFactory mf) {
        super(mf);
        if (FACTORY == null) {
            FACTORY = this.mf;
        }
    }

    @Override
    public GTESend<DynamicActionKind> DynamicGTESend(Role peer, MsgId<?> mid, Payload pay, int c, int n) {
        return new GTESend<>(MActionBase.DYNAMIC_ID, this.mf, peer, mid, pay, c, n);
    }

    @Override
    public GTERecv<DynamicActionKind> DynamicGTERecv(Role peer, MsgId<?> mid, Payload pay, int c, int n) {
        return new GTERecv<>(MActionBase.DYNAMIC_ID, this.mf, peer, mid, pay, c, n);
    }

    @Override
    public GTENewTimeout<DynamicActionKind> DynamicGTENewTimeout(int c, int n) {
        return new GTENewTimeout<>(MActionBase.DYNAMIC_ID, this.mf, c, n);
    }
}
