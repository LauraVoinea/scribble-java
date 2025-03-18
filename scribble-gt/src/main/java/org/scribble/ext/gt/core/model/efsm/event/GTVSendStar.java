package org.scribble.ext.gt.core.model.efsm.event;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;

public class GTVSendStar implements GTVAction {

    public final Role role;
    public final Op op;
    public final Payload pay;


    public GTVSendStar(Role role, Op op, Payload payload) {
        this.role = role;
        this.op = op;
        this.pay = payload;

    }

    @Override
    public String toString() {
        return this.role + "!*" + this.op;
    }

    @Override
    public int hashCode() {
        int hash = 15493;
        hash = 31 * hash + this.role.hashCode();
        hash = 31 * hash + this.op.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTVSendStar cast)) {
            return false;
        }
        return this.role.equals(cast.role) && this.op.equals(cast.op);
    }
}
