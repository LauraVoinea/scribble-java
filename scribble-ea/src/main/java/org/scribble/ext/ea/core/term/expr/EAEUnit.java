package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVUnitType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EAEUnit implements EAExpr {

    public static final EAEUnit UNIT = new EAEUnit();

    public EAEUnit() {
    }

    @Override
    public EAVUnitType infer() {
        return EAVUnitType.UNIT;
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
    public EAVUnitType type(Gamma gamma) {
        return EATypeFactory.factory.val.unit();
    }

    /* Aux */

    @Override
    public Set<EAEVar> getFreeVars() {
        //return Collections.emptySet();
        return new HashSet<>();
    }

    @Override
    public EAEUnit subs(@NotNull Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAFuncName, EAERec> m) { return this; }

    @Override
    public String toString() {
        return "()";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAEUnit eaVar = (EAEUnit) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEUnit;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.UNIT;
        hash = 31 * hash;
        return hash;
    }
}
