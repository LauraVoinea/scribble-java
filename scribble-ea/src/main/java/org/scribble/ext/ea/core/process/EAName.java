package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class EAName implements EAValue {

    @NotNull public final String id;

    public EAName(@NotNull String id) {
        this.id = id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAName eaVar = (EAName) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAName;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.NAME;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
