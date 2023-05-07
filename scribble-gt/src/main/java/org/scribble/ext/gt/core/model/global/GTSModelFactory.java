package org.scribble.ext.gt.core.model.global;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.model.global.actions.SSend;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.global.action.GTSSend;

public interface GTSModelFactory extends SModelFactory {

    @Override
    default <A extends ActionKind> SSend<A> SSend(Role subj, Role obj, MsgId<?> mid, Payload pay) {
        throw new RuntimeException("Deprecated");
    }

    @Override
    default <A extends ActionKind> SRecv<A> SRecv(Role subj, Role obj, MsgId<?> mid, Payload pay) {
        throw new RuntimeException("Deprecated");
    }

    <A extends ActionKind> SSend<A> GTSSend(Role subj, Role obj, MsgId<?> mid, Payload pay, int c, int n);

    <A extends ActionKind> SRecv<A> GTSRecv(Role subj, Role obj, MsgId<?> mid, Payload pay, int c, int n);

    <A extends ActionKind> GTSNewTimeout<A> SNewTimeout(int c, int n);
}
