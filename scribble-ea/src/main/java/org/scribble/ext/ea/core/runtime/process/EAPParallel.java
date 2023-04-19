package org.scribble.ext.ea.core.runtime.process;

import org.jetbrains.annotations.NotNull;

import java.util.*;

@Deprecated
public class EAPParallel implements EAProcess {

    @NotNull
    public final List<EAProcess> terms;

    protected EAPParallel(@NotNull List<EAProcess> terms) {
        this.terms = Collections.unmodifiableList(new LinkedList<>(terms));
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPParallel them = (EAPParallel) o;
        return them.canEquals(this)
                && this.terms.equals(them.terms);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPParallel;
    }

    @Override
    public int hashCode() {
        int hash = EAProcess.PARALLEL;
        hash = 31 * hash + this.terms.hashCode();
        return hash;

    }
}
