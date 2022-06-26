package org.scribble.ext.ea.core.config;

import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.EAPExpr;
import org.scribble.ext.ea.core.process.EAPTerm;

public class EAPActiveThread implements EAPThreadState {

    public final EAPExpr expr;
    public final EAPSid sid;
    public final Role role;

    protected EAPActiveThread(EAPExpr expr, EAPSid sid, Role role) {
        this.expr = expr;
        this.sid = sid;
        this.role = role;
    }

    /* aux */

    @Override
    public String toString() {
        return "(" + this.expr + ")@" + this.sid + "[" + this.role + "]";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPActiveThread eaVar = (EAPActiveThread) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPActiveThread;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.ACTIVE_THREAD;
        hash = 31 * hash;
        hash = 31 * this.expr.hashCode();
        hash = 31 * this.sid.hashCode();
        hash = 31 * this.role.hashCode();
        return hash;
    }
}
