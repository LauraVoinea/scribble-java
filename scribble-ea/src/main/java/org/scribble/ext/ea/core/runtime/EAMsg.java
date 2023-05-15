package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.value.EAVType;

public class EAMsg {

    @NotNull public final Role snd;
    @NotNull public final Role rcv;
    @NotNull public final Op op;
    @NotNull public final EAExpr data;

    public EAMsg(Role snd, Role rcv, Op op, EAExpr data) {
        this.snd = snd;
        this.rcv = rcv;
        this.op = op;
        this.data = data;
    }

    /* ... */

    @Override
    public String toString() {
        return "(" + this.snd + ", " + this.rcv + ", " + this.op + ", " + this.data + ")";
    }

    /* equals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMsg them = (EAMsg) o;
        return this.snd.equals(them.snd) && this.rcv.equals(them.rcv)
                && this.op.equals(them.op) && this.data.equals(them.data);
    }

    @Override
    public int hashCode() {
        int hash = 84979;
        hash = 31 * hash + this.snd.hashCode();
        hash = 31 * hash + this.rcv.hashCode();
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.data.hashCode();
        return hash;
    }
}
