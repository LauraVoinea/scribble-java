package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAPVar implements EAPVal, EAName {

    public final String id;

    public EAPVar(String id) {
        this.id = id;
    }

    /* Aux */

    @Override
    public EAValType type(Gamma gamma) {
        if (gamma.map.containsKey(this)) {
            return gamma.map.get(this);
        } else {
            EAPFuncName fhack = new EAPFuncName(this.id);
            if (gamma.fmap.containsKey(fhack)) {  // HERE FIXME fnames parsed as vars...
                return gamma.fmap.get(fhack);
            }
        }
        throw new RuntimeException("Unknown var: " + this + ", " + gamma);
    }

    @Override
    public boolean isGround(Set<EAPFuncName> fnames) {
        return fnames.contains(new EAPFuncName(this.id));
    }

    @Override
    public EAPVal subs(@NotNull Map<EAPVar, EAPVal> m) {
        return m.containsKey(this) ? m.get(this) : this;
    }

    @Override
    public EAPVal fsubs(Map<EAPFuncName, EAPRec> m) { return this; }

    @Override
    public Set<EAPVar> getFreeVars() {
        //return Set.of(this);
        return new HashSet<>(Set.of(this));
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
        EAPVar eaVar = (EAPVar) o;
        return eaVar.canEquals(this) && Objects.equals(this.id, eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPVar;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.VAR;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
