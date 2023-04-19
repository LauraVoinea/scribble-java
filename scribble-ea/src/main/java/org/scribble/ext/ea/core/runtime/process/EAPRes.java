package org.scribble.ext.ea.core.runtime.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.EAPid;

@Deprecated
public class EAPRes implements EAProcess {

    @NotNull
    public final EAPid name;  // !!! pid vs. "name"?  "a" vs. "c"?
    @NotNull
    public final EAProcess term;

    protected EAPRes(@NotNull EAPid name, @NotNull EAProcess term) {
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
        int hash = EAProcess.RESTRICTION;
        hash = 31 * hash + this.name.hashCode();
        hash = 31 * hash + this.term.hashCode();
        return hash;

    }
}
