package org.scribble.ext.assrt.core.type.formal.global.action;

import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.global.AssrtFormalGType;
import org.scribble.ext.assrt.core.type.session.AssrtMsg;

public class AssrtFormalGComm implements AssrtFormalGAction {

    public final Role src;
    public final Role dst;
    public final AssrtMsg msg;

    public AssrtFormalGComm(Role src, Role dst, AssrtMsg msg) {
        this.src = src;
        this.dst = dst;
        this.msg = msg;
    }

    @Override
    public String toString() {
        return this.src + "->" + this.dst + ":" + this.msg;
    }

    @Override
    public int hashCode() {
        int hash = AssrtFormalGType.COMM_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.msg.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof AssrtFormalGComm)) {
            return false;
        }
        AssrtFormalGComm them = (AssrtFormalGComm) o;
        return //them.canEquals(this) &&
                this.src.equals(them.src) && this.dst.equals(them.dst)
                        && this.msg.equals(them.msg);
    }

    //public abstract boolean canEquals(Object o);
}
