package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.process.EAProcess;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

// s, s', ...
public class EASid implements EARuntimeName {

    @NotNull
    public final String id;

    protected EASid(@NotNull String id) {
        this.id = id;
    }

    @Override
    //public EAVType type(GammaState gamma) {
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        throw new RuntimeException("Deprecated?");  // !!!
    }

    /* Aux */

    @Override
    public String toString() {
        return this.id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EASid eaVar = (EASid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EASid;
    }

    @Override
    public int hashCode() {
        int hash = EAProcess.SESSION_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
