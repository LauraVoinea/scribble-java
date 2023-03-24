package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAIntType;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Collections;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EAPUnit implements EAPVal {

    public static final EAPUnit UNIT = new EAPUnit();

    public EAPUnit() {
    }

    @Override
    public EAUnitType infer() {
        return EAUnitType.UNIT;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPVal beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAUnitType type(Gamma gamma) {
        return EATypeFactory.factory.val.unit();
    }

    /* Aux */

    @Override
    public Set<EAPVar> getFreeVars() {
        //return Collections.emptySet();
        return new HashSet<>();
    }

    @Override
    public EAPUnit subs(@NotNull Map<EAPVar, EAPVal> m) {
        return this;
    }

    @Override
    public EAPVal fsubs(Map<EAPFuncName, EAPRec> m) { return this; }

    @Override
    public String toString() {
        return "()";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPUnit eaVar = (EAPUnit) o;
        return eaVar.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPUnit;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.UNIT;
        hash = 31 * hash;
        return hash;
    }
}
