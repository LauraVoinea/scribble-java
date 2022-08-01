package org.scribble.ext.ea.core.type.value;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.EAType;
import org.scribble.ext.ea.core.type.session.local.EALType;

public class EAHandlersType implements EAValType {

    @NotNull public final EALType S;

    protected EAHandlersType(@NotNull EALType S) {
        this.S = S;
    }

    /* Aux */

    @Override
    public String toString() {
        return "Handler(" + this.S + "):";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAHandlersType them = (EAHandlersType) o;
        return them.canEquals(this) && this.S.equals(them.S);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAHandlersType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.HANDLERS;
        hash = 31 * hash + this.S.hashCode();
        return hash;
    }
}
