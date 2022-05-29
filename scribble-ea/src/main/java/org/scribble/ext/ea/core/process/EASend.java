package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Objects;

public class EASend implements EAExpr {

    @NotNull public final EAExpr expr;

    public EASend(@NotNull EAExpr expr) {
        this.expr = expr;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EASend eaVar = (EASend) o;
        return eaVar.canEquals(this)
                && this.expr.equals(eaVar.expr);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EASend;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.SEND;
        hash = 31 * hash + this.expr.hashCode();
        return hash;
    }
}
