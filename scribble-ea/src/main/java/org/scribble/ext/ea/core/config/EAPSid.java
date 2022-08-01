package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.process.EAPTerm;
import org.scribble.ext.ea.core.process.EAPVal;
import org.scribble.ext.ea.core.process.EAPVar;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

// s, s', ...
public class EAPSid implements EAPVal, EARuntimeName {

    @NotNull public final String id;

    protected EAPSid(@NotNull String id) {
        this.id = id;
    }

    @Override
    public EAValType type(Gamma gamma) {
        throw new RuntimeException("CHECKME: N/A ?");  // !!!
    }

    /* Aux */

    @Override
    public EAPSid subs(@NotNull Map<EAPVar, EAPVal> m) {
        return this;
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        return Collections.emptySet();
    }

    @Override
    public String toString() {
        return this.id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSid eaVar = (EAPSid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSid;
    }

    @Override
    public int hashCode() {
        int hash = EAPRuntimeTerm.SESSION_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
