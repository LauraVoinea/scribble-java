package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Set;

public class EAPSend implements EAPExpr {

    @NotNull public final EAPVal val;  // value, not expr

    public EAPSend(@NotNull EAPVal val) {
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
    public Set<EAPVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    @Override
    public EAPSend subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.send(val1);
    }

    @Override
    public String toString() {
        return "send " + this.val;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSend eaVar = (EAPSend) o;
        return eaVar.canEquals(this)
                && this.val.equals(eaVar.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSend;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.SEND;
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
