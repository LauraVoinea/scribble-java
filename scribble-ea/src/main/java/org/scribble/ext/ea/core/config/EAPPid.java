package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.process.*;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// a, b, ...
public class EAPPid implements EAPVal, EARuntimeName {

    @NotNull
    public final String id;

    public EAPPid(@NotNull String id) {
        this.id = id;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPVal beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAValType type(Gamma gamma) {
        return EATypeFactory.factory.val.pid();
    }

    /* Aux */

    @Override
    public EAPPid subs(@NotNull Map<EAPVar, EAPVal> m) {
        return this;
    }

    @Override
    public EAPVal fsubs(Map<EAPFuncName, EAPRec> m) { return this; }

    @Override
    public Set<EAPVar> getFreeVars() {
        //return Collections.emptySet();
        return new HashSet<>();
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
        EAPPid eaVar = (EAPPid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPPid;
    }

    @Override
    public int hashCode() {
        int hash = EAPRuntimeTerm.PROCESS_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
