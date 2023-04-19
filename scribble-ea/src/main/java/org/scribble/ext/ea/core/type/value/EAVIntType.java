package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EAVIntType implements EAVType {

    public static final EAVIntType INT = new EAVIntType();

    protected EAVIntType() {
    }

    /* Aux */

    @Override
    public String toString() {
        return "Int";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAVIntType them = (EAVIntType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVIntType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.INT;
        hash = 31 * hash;
        return hash;
    }
}
