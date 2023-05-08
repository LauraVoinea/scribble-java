package org.scribble.ext.gt.core.model.local.action;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.action.GTSRecv;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;

public class GTERecv<A extends ActionKind> extends ERecv<A> implements GTEAction {

    public final int c;
    public final int n;

    // peer is receiver
    public GTERecv(int id, ModelFactory ef, Role peer, MsgId<?> mid, Payload pay,
                   int c, int n) {
        super(id, ef, peer, mid, pay);
        this.c = c;
        this.n = n;
    }

    /* ... */

    @Override
    public GTERecv<DynamicActionKind> toDynamic() {
        return ((GTEModelFactory) this.mf.local)
                .DynamicGTERecv(this.peer, this.mid, this.payload, this.c, this.n);
    }

    @Override
    public GTESend<DynamicActionKind> toDynamicDual(Role self) {
        return ((GTEModelFactory) this.mf.local)
                .DynamicGTESend(self, this.mid, this.payload, this.c, this.n);
    }

    @Override
    public GTSRecv<StaticActionKind> toStaticGlobal(Role self) {
        return ((GTSModelFactory) this.mf.global)
                .GTSRecv(self, this.peer, this.mid, this.payload, this.c, this.n);

    }

    /* ... */

    @Override
    public int getC() {
        return this.c;
    }

    @Override
    public int getN() {
        return this.n;
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 14969;
        hash = 31 * hash + super.hashCode();
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.n;
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTERecv)) {
            return false;
        }
        GTERecv them = (GTERecv) o;
        return super.equals(o) && this.c == them.c && this.n == them.n;  // Does canEquals
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTERecv;
    }

}
