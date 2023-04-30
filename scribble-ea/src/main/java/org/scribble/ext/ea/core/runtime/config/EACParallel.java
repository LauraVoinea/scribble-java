package org.scribble.ext.ea.core.runtime.config;

import org.jetbrains.annotations.NotNull;

import java.util.*;

@Deprecated
public class EACParallel implements EAConfig {

    @NotNull
    public final List<EAConfig> terms;

    protected EACParallel(@NotNull List<EAConfig> terms) {
        this.terms = Collections.unmodifiableList(new LinkedList<>(terms));
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EACParallel them = (EACParallel) o;
        return them.canEquals(this)
                && this.terms.equals(them.terms);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EACParallel;
    }

    @Override
    public int hashCode() {
        int hash = EAConfig.PARALLEL;
        hash = 31 * hash + this.terms.hashCode();
        return hash;

    }
}
