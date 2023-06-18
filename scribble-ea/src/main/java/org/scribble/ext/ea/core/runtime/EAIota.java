package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.config.EAConfig;

// i, i', ...
public class EAIota implements EARuntimeName {

    @NotNull public final String id;

    public EAIota(String id) {
        this.id = id;
    }

    /*public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        throw new RuntimeException("Deprecated?");
    }*/

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
        EAIota them = (EAIota) o;
        return them.canEquals(this) && this.id.equals(them.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAIota;
    }

    @Override
    public int hashCode() {
        int hash = EAConfig.IOTA_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
