package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

public class EAReturn implements EAExpr {

    @NotNull
    public final EAExpr expr;

    public EAReturn(EAExpr expr) {
        this.expr = expr;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAReturn eaVar = (EAReturn) o;
        return eaVar.canEquals(this)
                && this.expr.equals(eaVar.expr);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAReturn;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.RETURN;
        hash = 31 * hash + this.expr.hashCode();
        return hash;
    }
}
