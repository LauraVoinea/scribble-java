package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.EAFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAEVar implements EAExpr, EAName {

    public final String id;

    public EAEVar(String id) {
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

    /* Aux */

    @Override
    public EAVType type(Gamma gamma) {
        if (gamma.map.containsKey(this)) {
            return gamma.map.get(this);
        }
        throw new RuntimeException("Unknown var: " + this + ", " + gamma);
    }

    @Override
    public boolean isGround() {
        return false;
    }

    @Override
    public EAExpr subs(@NotNull Map<EAEVar, EAExpr> m) {
        return m.containsKey(this) ? m.get(this) : this;
    }

    @Override
    public EAExpr fsubs(Map<EAFuncName, EAERec> m) { return this; }

    @Override
    public Set<EAEVar> getFreeVars() {
        //return Set.of(this);
        return new HashSet<>(Set.of(this));
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
        EAEVar eaVar = (EAEVar) o;
        return eaVar.canEquals(this) && Objects.equals(this.id, eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEVar;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.VAR;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
