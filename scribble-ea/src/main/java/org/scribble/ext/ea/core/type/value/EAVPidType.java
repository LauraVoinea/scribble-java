package org.scribble.ext.ea.core.type.value;

import org.scribble.ext.ea.core.type.EAType;

public class EAVPidType implements EAVType {

    public static final EAVPidType PID = new EAVPidType();

    protected EAVPidType() {
    }

    /* Aux */

    @Override
    public String toString() {
        return "Pid";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAVPidType them = (EAVPidType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAVPidType;
    }

    @Override
    public int hashCode() {
        int hash = EAType.PID;
        hash = 31 * hash;
        return hash;
    }
}
