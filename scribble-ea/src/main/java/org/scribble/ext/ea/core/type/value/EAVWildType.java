package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EAVWildType implements EAVType {

    public static final EAVWildType WILD = new EAVWildType();

    protected EAVWildType() {
    }

    /* Aux */

    @Override
    public String toString() {
        return "_";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAVWildType them = (EAVWildType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVWildType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.WILD;
        hash = 31 * hash;
        return hash;
    }
}
