package org.scribble.ext.ea.core.config;

import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.process.EAPExpr;
import org.scribble.ext.ea.core.process.EAPTerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

public class EAPActiveThread implements EAPThreadState {

    public final EAPExpr expr;
    public final EAPSid sid;
    public final Role role;

    protected EAPActiveThread(EAPExpr expr, EAPSid sid, Role role) {
        this.expr = expr;
        this.sid = sid;
        this.role = role;
    }

    // [TT-Sess]
    @Override
    public void type(Gamma gamma, Delta delta) {
        if (delta.map.size() != 1) {
            throw new RuntimeException("Invalid Delta: " + delta);
        }
        EALType pre = delta.map.get(new Pair<>(this.sid, this.role));
        if (pre == null) {
            throw new RuntimeException("Unknown endpoint: "
                    + endpointToString(this.sid, this.role));
        }
        Pair<EAValType, EALType> res = this.expr.type(gamma, pre);
        if (!res.equals(new EAPPair<>(EAUnitType.UNIT, EALEndType.END))) {
            throw new RuntimeException("Badly typed: " + this + " : "
                    + res.left + " <| " + res.right);
        }
    }

    /* aux */

    public static String endpointToString(EAPSid sid, Role role) {
        return sid + "[" + role + "]";
    }

    @Override
    public String toString() {
        return "(" + this.expr + ")@" + endpointToString(this.sid, this.role);
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
