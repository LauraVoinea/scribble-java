package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.local.LType;

import java.util.Map;
import java.util.Optional;

public class EALRecVarType implements EALType {

    @NotNull public final RecVar var;

    protected EALRecVarType(@NotNull RecVar var) {
        this.var = var;
    }

    @Override
    public EALType concat(EALType t) {
        throw new RuntimeException("Concat only defined for unary send");
    }

    @Override
    public EALType subs(Map<RecVar, EALRecType> map) {
        return map.containsKey(this.var) ? map.get(this.var) : this;
    }

    @Override
    public EALType unfoldAllOnce() {
        return this;
    }

    /*@Override
    public EALRecVarType unfold() {
        return this;
    }*/

    @Override
    public EALType unfold(RecVar rvar, EALType t) {
        return rvar.equals(this.var) ? t : this;
    }

    @Override
    public Optional<EALType> step(LType a) {
        return Optional.empty();
    }

    /* Aux */

    @Override
    public String toString() {
        return this.var.toString();
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EALRecVarType them = (EALRecVarType) o;
        return them.canEquals(this) && this.var.equals(them.var);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EALRecVarType;
    }

    @Override
    public int hashCode() {
        int hash = EALType.RECVAR_HASH;
        hash = 31 * hash + this.var.hashCode();
        return hash;
    }
}
