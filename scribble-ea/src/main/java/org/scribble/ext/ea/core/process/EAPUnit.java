package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

public class EAPUnit implements EAPVal {

    public static final EAPUnit UNIT = new EAPUnit();

    public EAPUnit() {
    }

    /* Aux */

    @Override
    public Set<EAPVar> getFreeVars() {
        return Collections.emptySet();
    }

    @Override
    public EAPUnit subs(@NotNull Map<EAPVar, EAPVal> m) {
         return this;
    }

    @Override
    public String toString() {
        return "()";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPUnit eaVar = (EAPUnit) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPUnit;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.UNIT;
        hash = 31 * hash;
        return hash;
    }
}
