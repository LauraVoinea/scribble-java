package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EAIntType implements EAValType {

    public static final EAIntType INT = new EAIntType();

    protected EAIntType() {
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
        EAIntType them = (EAIntType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAIntType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.INT;
        hash = 31 * hash;
        return hash;
    }
}
