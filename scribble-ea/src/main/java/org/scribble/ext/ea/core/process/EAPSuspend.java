package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.Map;
import java.util.Set;

public class EAPSuspend implements EAPExpr {

    @NotNull
    public final EAPVal val;  // value, not expr

    public EAPSuspend(EAPVal val) {
        this.val = val;
    }

    @Override
    public EAPSuspend subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.suspend(val1);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSuspend eaVar = (EAPSuspend) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSuspend;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.SUSPEND;
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
