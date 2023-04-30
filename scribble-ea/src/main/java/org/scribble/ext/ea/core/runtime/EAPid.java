package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.config.EAConfig;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

// a, b, ...
public class EAPid implements EARuntimeName {

    @NotNull
    public final String id;

    public EAPid(@NotNull String id) {
        this.id = id;
    }

    @Override
    //public EAVType type(GammaState gamma) {
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        //return Either.right(new Pair<>(EATypeFactory.factory.val.pid(), new Tree<>("..pid..")));
        throw new RuntimeException("Deprecated?");
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
        EAPid eaVar = (EAPid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPid;
    }

    @Override
    public int hashCode() {
        int hash = EAConfig.PROCESS_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
