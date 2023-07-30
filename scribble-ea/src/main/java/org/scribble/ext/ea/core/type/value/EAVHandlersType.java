package org.scribble.ext.ea.core.type.value;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.EAType;
import org.scribble.ext.ea.core.type.session.local.EALType;

public class EAVHandlersType implements EAVType {

    @NotNull public final EALType S;
    @NotNull public final EAVType T;  // state

    protected EAVHandlersType(EALType S, EAVType T) {
        this.S = S;
        this.T = T;
    }

    /* Aux */

    @Override
    public String toString() {
        return "Handler(" + this.T + ", " + this.S + ")";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }
        EAVHandlersType them = (EAVHandlersType) o;
        return them.canEquals(this) && this.S.equals(them.S) && this.T.equals(them.T);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVHandlersType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.HANDLERS;
        hash = 31 * hash + this.S.hashCode();
        hash = 31 * hash + this.T.hashCode();
        return hash;
    }
}
