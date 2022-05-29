package org.scribble.ext.ea.core.process;

import java.util.Objects;

public class EAVar implements EAValue {

    public final String id;

    public EAVar(String id) {
        this.id = id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAVar eaVar = (EAVar) o;
        return eaVar.canEquals(this) && Objects.equals(this.id, eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVar;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.VAR;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
