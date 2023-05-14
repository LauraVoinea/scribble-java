package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVBoolType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

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

    @Override
    public Either<Exception, Pair<EAExpr, Tree<String>>> eval() {
        return Either.left(newStuck());
    }

    /* Aux */

    @Override
    //public EAVType type(GammaState gamma) {
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        return Either.right(Pair.of(
                EAVBoolType.BOOL,
                new Tree<>("[TV-Unit] " + toTypeJudgeString(gamma, EAVBoolType.BOOL))
        ));
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
