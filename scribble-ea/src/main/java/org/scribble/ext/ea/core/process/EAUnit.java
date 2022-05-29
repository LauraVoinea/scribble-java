package org.scribble.ext.ea.core.process;

import java.util.Objects;

public class EAUnit implements EAValue {

    public static final EAUnit UNIT = new EAUnit();

    public EAUnit() {
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAUnit eaVar = (EAUnit) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAUnit;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.UNIT;
        hash = 31 * hash;
        return hash;
    }
}
