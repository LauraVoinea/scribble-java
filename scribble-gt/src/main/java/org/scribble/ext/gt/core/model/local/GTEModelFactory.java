package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.model.global.actions.SSend;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.global.action.GTSRecv;
import org.scribble.ext.gt.core.model.global.action.GTSSend;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.core.model.local.action.GTERecv;
import org.scribble.ext.gt.core.model.local.action.GTESend;

public interface GTEModelFactory extends EModelFactory {

    @Override
    default ESend<StaticActionKind> StaticESend(Role peer, MsgId<?> mid, Payload pay) {
        throw new RuntimeException("Deprecated");
    }

    @Override
    default ERecv<StaticActionKind> StaticERecv(Role peer, MsgId<?> mid, Payload pay) {
        throw new RuntimeException("Deprecated");
    }

    @Override
    default ESend<DynamicActionKind> DynamicESend(Role peer, MsgId<?> mid, Payload pay) {
        throw new RuntimeException("Deprecated");
    }

    @Override
    default ERecv<DynamicActionKind> DynamicERecv(Role peer, MsgId<?> mid, Payload pay) {
        throw new RuntimeException("Deprecated");
    }

    GTESend<DynamicActionKind> DynamicGTESend(Role peer, MsgId<?> mid, Payload pay, int c, int n);

    GTERecv<DynamicActionKind> DynamicGTERecv(Role peer, MsgId<?> mid, Payload pay, int c, int n);

    GTENewTimeout<DynamicActionKind> DynamicGTENewTimeout(int c, int n);
}
