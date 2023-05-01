package org.scribble.ext.ea.core.runtime.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;

import java.util.*;

@Deprecated
public class EACParallel implements EAConfig {

    @NotNull
    public final List<EAConfig> terms;

    protected EACParallel(@NotNull List<EAConfig> terms) {
        this.terms = Collections.unmodifiableList(new LinkedList<>(terms));
    }

    @Override
    public Either<Exception, Tree<String>> type(Gamma gamma, Delta delta) {
        throw new RuntimeException("TODO");
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
