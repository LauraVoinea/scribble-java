package org.scribble.ext.ea.core.runtime.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.EAPid;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.Delta;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;

@Deprecated
public class EACRes implements EAConfig {

    @NotNull
    public final EAPid name;  // !!! pid vs. "name"?  "a" vs. "c"?
    @NotNull
    public final EAConfig term;

    protected EACRes(@NotNull EAPid name, @NotNull EAConfig term) {
        this.name = name;
        this.term = term;
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
        EACRes them = (EACRes) o;
        return them.canEquals(this)
                && this.name.equals(them.name)
                && this.term.equals(them.term);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EACRes;
    }

    @Override
    public int hashCode() {
        int hash = EAConfig.RESTRICTION;
        hash = 31 * hash + this.name.hashCode();
        hash = 31 * hash + this.term.hashCode();
        return hash;

    }
}
