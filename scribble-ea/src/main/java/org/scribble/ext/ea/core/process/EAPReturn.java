package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Set;

public class EAPReturn implements EAPExpr {

    @NotNull
    public final EAPVal val;  // value, not expr

    public EAPReturn(EAPVal val) {
        this.val = val;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /* Aux */

    @Override
    public EAPReturn subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.returnn(val1);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    @Override
    public String toString() {
        return "return " + this.val;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPReturn eaVar = (EAPReturn) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPReturn;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.RETURN;
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
