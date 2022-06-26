package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.process.EAPSend;
import org.scribble.ext.ea.core.process.EAPTerm;

import java.util.*;
import java.util.stream.Collectors;

@Deprecated
public class EAPParallel implements EAPRuntimeTerm {

    @NotNull public final List<EAPRuntimeTerm> terms;

    protected EAPParallel(@NotNull List<EAPRuntimeTerm> terms) {
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
        int hash = EAPRuntimeTerm.PARALLEL;
        hash = 31 * hash + this.terms.hashCode();
        return hash;

    }
}
