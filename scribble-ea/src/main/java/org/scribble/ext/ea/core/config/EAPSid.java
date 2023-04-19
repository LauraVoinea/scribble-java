package org.scribble.ext.ea.core.config;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAPRec;
import org.scribble.ext.ea.core.term.expr.EAPExpr;
import org.scribble.ext.ea.core.term.expr.EAPVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// s, s', ...  // FIXME no longer values (not EAPVal)
public class EAPSid implements EAPExpr, EARuntimeName {

    @NotNull
    public final String id;

    protected EAPSid(@NotNull String id) {
        this.id = id;
    }

    @Override
    public EAValType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public EAValType type(Gamma gamma) {
        throw new RuntimeException("CHECKME: N/A ?");  // !!!
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
    public EAPSid subs(@NotNull Map<EAPVar, EAPExpr> m) {
        return this;
    }

    @Override
    public EAPExpr fsubs(Map<EAPFuncName, EAPRec> m) { return this; }

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
        EAPSid eaVar = (EAPSid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSid;
    }

    @Override
    public int hashCode() {
        int hash = EAPRuntimeTerm.SESSION_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
