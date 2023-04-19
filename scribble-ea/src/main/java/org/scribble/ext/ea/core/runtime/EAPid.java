package org.scribble.ext.ea.core.runtime;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.process.EAProcess;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// a, b, ...  // TODO no longer EAPVal
public class EAPid implements EAExpr, EARuntimeName {

    @NotNull
    public final String id;

    public EAPid(@NotNull String id) {
        this.id = id;
    }

    @Override
    public EAVType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAVType type(Gamma gamma) {
        return EATypeFactory.factory.val.pid();
    }

    /* Aux */

    @Override
    public EAPid subs(@NotNull Map<EAEVar, EAExpr> m) {
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
        EAPid eaVar = (EAPid) o;
        return eaVar.canEquals(this) && this.id.equals(eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPid;
    }

    @Override
    public int hashCode() {
        int hash = EAProcess.PROCESS_ID;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
