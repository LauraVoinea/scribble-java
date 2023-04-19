package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EAVUnitType implements EAVType {

    public static final EAVUnitType UNIT = new EAVUnitType();

    protected EAVUnitType() {
    }

    /* Aux */

    @Override
    public String toString() {
        return "1";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAVUnitType them = (EAVUnitType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVUnitType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.UNIT;
        hash = 31 * hash;
        return hash;
    }
}
