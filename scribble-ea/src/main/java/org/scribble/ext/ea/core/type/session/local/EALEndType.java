package org.scribble.ext.ea.core.type.session.local;

import org.intellij.lang.annotations.JdkConstants;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.session.local.LType;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Optional;

public class EALEndType implements EALType {

    public static final EALEndType END = new EALEndType();

    protected EALEndType() {
    }

    @Override
    public EALType concat(EALType t) {
        return t;
    }

    @Override
    public Optional<EALType> step(LType a) {
        return Optional.empty();
    }

    /* Aux */

    @Override
    public String toString() {
        return "end";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EALEndType them = (EALEndType) o;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EALEndType;
    }

    @Override
    public int hashCode() {
        int hash = EALType.END;
        hash = 31 * hash;
        return hash;
    }
}
