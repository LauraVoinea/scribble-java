package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

// "Runtime names"
public class EAPPid implements EAPVal {

    @NotNull public final String id;

    public EAPPid(@NotNull String id) {
        this.id = id;
    }

    /* Aux */

    @Override
    public EAPPid subs(@NotNull Map<EAPVar, EAPVal> m) {
        return this;
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        return Collections.emptySet();
    }

    @Override
    public String toString() {
        return this.id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPPid eaVar = (EAPPid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPPid;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.NAME;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
