package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EAFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVIntType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAEIntVal implements EAExpr {

    public final int val;

    public EAEIntVal(int val) {
        this.val = val;
    }

    @Override
    public EAVIntType infer() {
        return EAVIntType.INT;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /*@Override
    public EAPBExpr recon(@NotNull EAPBExpr old, EAPBExpr neww) {
        return this.equals(old) ? neww : this;
    }*/

    /* Aux */

    @Override
    public EAVType type(Gamma gamma) {
        return EAVIntType.INT;
    }

    @Override
    public EAExpr subs(Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAFuncName, EAERec> m) {
        return this;
    }

    @Override
    public boolean isGround() {
        return true;
    }

    @Override
    public Set<EAEVar> getFreeVars() {
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
        EAEIntVal them = (EAEIntVal) o;
        return them.canEquals(this) && Objects.equals(this.val, them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEIntVal;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.INT;
        hash = 31 * hash + this.val;
        return hash;
    }
}
