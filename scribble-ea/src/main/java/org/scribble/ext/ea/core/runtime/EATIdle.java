package org.scribble.ext.ea.core.runtime;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;

public class EATIdle implements EAThread {

    public static final EATIdle IDLE = new EATIdle();

    protected EATIdle() {
    }

    @Override
    public boolean isIdle() {
        return true;
    }

    // [TT-Idle]
    @Override
    public Either<Exception, Tree<String>> type(GammaState gamma, Delta delta) {
        if (!delta.map.isEmpty()) {
            //throw new RuntimeException("Invalid Delta: " + delta);
            return Either.left(new Exception("Invalid Delta: " + delta));
        }
        return Either.right(new Tree<>("[TT-Idle]"));
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
        EATIdle eaVar = (EATIdle) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EATIdle;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.IDLE;
        hash = 31 * hash;
        return hash;
    }
}
