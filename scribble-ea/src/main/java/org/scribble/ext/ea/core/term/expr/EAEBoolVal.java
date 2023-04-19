package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVBoolType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAEBoolVal implements EAExpr {

    public final boolean val;

    public EAEBoolVal(boolean val) {
        this.val = val;
    }

    @Override
    public EAVBoolType infer() {
        return EAVBoolType.BOOL;
    }

    /*@Override
    public EAPBExpr recon(@NotNull EAPBExpr old, EAPBExpr neww) {
        return this.equals(old) ? neww : this;
    }*/

    /* Aux */

    @Override
    public EAVType type(Gamma gamma) {
        return EAVBoolType.BOOL;
    }

    @Override
    public EAExpr subs(Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAEFuncName, EAERec> m) {
        return this;
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        return new HashSet<>();
    }

    @Override
    public boolean isValue() {
        return true;  // though only in body of rec f in an app, where beta_M will subs it for rec f ...
    }

    /*@Override
    public boolean isGround() {
        return true;
    }*/

    @Override
    public String toString() {
        return Boolean.toString(this.val);
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAEBoolVal them = (EAEBoolVal) o;
        return them.canEquals(this) && Objects.equals(this.val, them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEBoolVal;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.BOOL;
        hash = 31 * hash + (this.val ? 1 : 0);
        return hash;
    }
}
