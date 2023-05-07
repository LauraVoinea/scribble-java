package org.scribble.ext.gt.core.model.local.action;

import org.scribble.core.model.ActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.MsgId;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

public class GTESend<A extends ActionKind> extends ESend<A> implements GTEAction {

    public final int c;
    public final int n;

    // MActionBase.DYNAMIC_ID for dynamic
    // peer is receiver
    public GTESend(int id, ModelFactory ef, Role peer, MsgId<?> mid, Payload pay,
                   int c, int n) {
        super(id, ef, peer, mid, pay);
        this.c = c;
        this.n = n;
    }

    @Override
    public String toString() {
        return "(" + this.obj + ", " + this.mid + ":" + this.payload
                + ", " + c + ", " + n + ")";
    }

    @Override
    public int hashCode() {
        int hash = 14957;
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
        if (!(o instanceof GTESend)) {
            return false;
        }
        GTESend them = (GTESend) o;
        return super.equals(o) && this.c == them.c && this.n == them.n;  // Does canEquals
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTESend;
    }

}
