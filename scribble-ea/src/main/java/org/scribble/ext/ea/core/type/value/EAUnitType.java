package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EAUnitType implements EAValType {

    public static final EAUnitType UNIT = new EAUnitType();

    protected EAUnitType() {
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
        EAUnitType them = (EAUnitType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAUnitType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.UNIT;
        hash = 31 * hash;
        return hash;
    }
}
