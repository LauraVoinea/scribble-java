package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.session.local.LType;

import java.util.Optional;

public class EALRecType implements EALType {

    @NotNull public final RecVar var;
    @NotNull public final EALType body;

    protected EALRecType(@NotNull RecVar var, @NotNull EALType body) {
        this.var = var;
        this.body = body;
    }

    @Override
    public EALType concat(EALType t) {
        throw new RuntimeException("Concat not defined for recursion");
    }

    public EALType unfold() {
        return unfold(this.var, this.body);
    }

    @Override
    public EALType unfold(RecVar rvar, EALType t) {
        return this.var.equals(rvar)
                ? this.body.unfold(rvar, t)  // Expected: this.body.equals(t)
                : new EALRecType(this.var, this.body.unfold(rvar, t));
    }

    @Override
    public Optional<EALType> step(LType a) {
        return unfold().step(a);
    }

    /* Aux */

    @Override
    public String toString() {
        return "mu " + this.var + "." + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EALRecType them = (EALRecType) o;
        return them.canEquals(this)
                && this.var.equals(them.var) && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EALRecType;
    }

    @Override
    public int hashCode() {
        int hash = EALType.REC_HASH;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }
}
