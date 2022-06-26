package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;

public class EAPRes implements EAPRuntimeTerm {

    @NotNull public final EAPPid name;  // !!! pid vs. "name"?  "a" vs. "c"?
    @NotNull public final EAPRuntimeTerm term;

    protected EAPRes(@NotNull EAPPid name, @NotNull EAPRuntimeTerm term) {
        this.name = name;
        this.term = term;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPRes them = (EAPRes) o;
        return them.canEquals(this)
                && this.name.equals(them.name)
                && this.term.equals(them.term);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPRes;
    }

    @Override
    public int hashCode() {
        int hash = EAPRuntimeTerm.RESTRICTION;
        hash = 31 * hash + this.name.hashCode();
        hash = 31 * hash + this.term.hashCode();
        return hash;

    }
}
