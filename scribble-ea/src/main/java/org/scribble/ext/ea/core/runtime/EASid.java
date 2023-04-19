package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.process.EAProcess;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// s, s', ...  // FIXME no longer values (not EAPVal)
public class EASid implements EAExpr, EARuntimeName {

    @NotNull
    public final String id;

    protected EASid(@NotNull String id) {
        this.id = id;
    }

    @Override
    public EAVType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public EAVType type(Gamma gamma) {
        throw new RuntimeException("CHECKME: N/A ?");  // !!!
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /* Aux */

    @Override
    public EASid subs(@NotNull Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAFuncName, EAERec> m) { return this; }

    @Override
    public Set<EAEVar> getFreeVars() {
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
        EASid eaVar = (EASid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EASid;
    }

    @Override
    public int hashCode() {
        int hash = EAProcess.SESSION_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
