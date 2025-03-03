package org.scribble.ext.gt.core.model.efsm.event;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

public class GTVSend implements GTVAction {

    public final Role role;
    public final Op op;
    public final Payload pay;

    public GTVSend(Role role, Op op, Payload pay) {
        this.role = role;
        this.op = op;
        this.pay = pay;
    }

    @Override
    public String toString() {
        return this.role + "!" + this.op + this.pay;
    }

    @Override
    public int hashCode() {
        int hash = 15467;
        hash = 31 * hash + this.role.hashCode();
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.pay.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTVSend cast)) {
            return false;
        }
        return this.role.equals(cast.role)
                && this.op.equals(cast.op) && this.pay.equals(pay);
    }
}
