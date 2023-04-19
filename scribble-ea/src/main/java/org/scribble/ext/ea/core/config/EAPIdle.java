package org.scribble.ext.ea.core.config;

import org.scribble.ext.ea.core.term.EAPTerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;

public class EAPIdle implements EAPThreadState {

    public static final EAPIdle IDLE = new EAPIdle();

    protected EAPIdle() {
    }

    @Override
    public boolean isIdle() {
        return true;
    }

    // [TT-Idle]
    @Override
    public void type(Gamma gamma, Delta delta) {
        if (!delta.map.isEmpty()) {
            throw new RuntimeException("Invalid Delta: " + delta);
        }
    }

    /* aux */

    @Override
    public String toString() {
        return "idle";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPIdle eaVar = (EAPIdle) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPIdle;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.IDLE;
        hash = 31 * hash;
        return hash;
    }
}
