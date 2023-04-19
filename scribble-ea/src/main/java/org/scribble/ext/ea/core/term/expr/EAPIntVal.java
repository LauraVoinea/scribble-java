package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EAPFuncName;
import org.scribble.ext.ea.core.term.EAPTerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAIntType;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAPIntVal implements EAPExpr {

    public final int val;

    public EAPIntVal(int val) {
        this.val = val;
    }

    @Override
    public EAIntType infer() {
        return EAIntType.INT;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /*@Override
    public EAPBExpr recon(@NotNull EAPBExpr old, EAPBExpr neww) {
        return this.equals(old) ? neww : this;
    }*/

    /* Aux */

    @Override
    public EAValType type(Gamma gamma) {
        return EAIntType.INT;
    }

    @Override
    public EAPExpr subs(Map<EAPVar, EAPExpr> m) {
        return this;
    }

    @Override
    public EAPExpr fsubs(Map<EAPFuncName, EAPRec> m) {
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
        return Integer.toString(this.val);
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPIntVal them = (EAPIntVal) o;
        return them.canEquals(this) && Objects.equals(this.val, them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPIntVal;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.INT;
        hash = 31 * hash + this.val;
        return hash;
    }
}
