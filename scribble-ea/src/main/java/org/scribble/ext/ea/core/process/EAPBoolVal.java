package org.scribble.ext.ea.core.process;

import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EABoolType;
import org.scribble.ext.ea.core.type.value.EAIntType;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAPBoolVal implements EAPVal {

    public final boolean val;

    public EAPBoolVal(boolean val) {
        this.val = val;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPVal beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /*@Override
    public EAPBExpr recon(@NotNull EAPBExpr old, EAPBExpr neww) {
        return this.equals(old) ? neww : this;
    }*/

    /* Aux */

    @Override
    public EAValType type(Gamma gamma) {
        return EABoolType.BOOL;
    }

    @Override
    public EAPVal subs(Map<EAPVar, EAPVal> m) {
        return this;
    }

    @Override
    public EAPVal fsubs(Map<EAPFuncName, EAPRec> m) {
        return this;
    }

    @Override
    public boolean isGround() {
        return true;
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        return new HashSet<>();
    }

    @Override
    public String toString() {
        return Boolean.toString(this.val);
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPBoolVal them = (EAPBoolVal) o;
        return them.canEquals(this) && Objects.equals(this.val, them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPBoolVal;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.BOOL;
        hash = 31 * hash + (this.val ? 1 : 0);
        return hash;
    }
}
