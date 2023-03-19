package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EABoolType implements EAValType {

    public static final EABoolType BOOL = new EABoolType();

    protected EABoolType() {
    }

    /* Aux */

    @Override
    public String toString() {
        return "Bool";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EABoolType them = (EABoolType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EABoolType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.BOOL;
        hash = 31 * hash;
        return hash;
    }
}
