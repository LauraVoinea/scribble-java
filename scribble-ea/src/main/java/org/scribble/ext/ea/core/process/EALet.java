package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class EALet implements EAExpr {

    @NotNull public final EAVar var;
    @NotNull public final EAExpr init;
    @NotNull public final EAExpr body;

    public EALet(@NotNull EAVar var, @NotNull EAExpr init, @NotNull EAExpr body) {
        this.var = var;
        this.init = init;
        this.body = body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EALet eaVar = (EALet) o;
        return eaVar.canEquals(this)
                && this.var.equals(eaVar.var)
                && this.init.equals(eaVar.init)
                && this.body.equals(eaVar.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EALet;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.LET;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.init.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }
}
