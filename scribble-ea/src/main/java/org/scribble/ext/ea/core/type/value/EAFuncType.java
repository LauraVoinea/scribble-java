package org.scribble.ext.ea.core.type.value;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.EAType;
import org.scribble.ext.ea.core.type.session.local.EALType;

public class EAFuncType implements EAValType {

    @NotNull public final EAValType A;
    @NotNull public final EAValType B;
    @NotNull public final EALType S;
    @NotNull public final EALType T;

    protected EAFuncType(@NotNull EAValType A, @NotNull EALType S,
                         @NotNull EALType T, @NotNull EAValType B) {
        this.S = S;
        this.A = A;
        this.B = B;
        this.T = T;
    }

    /* Aux */

    @Override
    public String toString() {
        return this.A + " " + this.S + " -> " + this.T + " " + this.B;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAFuncType them = (EAFuncType) o;
        return them.canEquals(this)
                && this.A.equals(them.A)
                && this.S.equals(them.S)
                && this.T.equals(them.T)
                && this.B.equals(them.B);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAFuncType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.HANDLERS;
        hash = 31 * hash + this.A.hashCode();
        hash = 31 * hash + this.S.hashCode();
        hash = 31 * hash + this.T.hashCode();
        hash = 31 * hash + this.B.hashCode();
        return hash;
    }
}
