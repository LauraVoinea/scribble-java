package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.process.EAProcess;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

// a, b, ...
public class EAPid implements EARuntimeName {

    @NotNull
    public final String id;

    public EAPid(@NotNull String id) {
        this.id = id;
    }

    @Override
    public EAVType type(Gamma gamma) {
        return EATypeFactory.factory.val.pid();
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
        int hash = EAProcess.PROCESS_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
