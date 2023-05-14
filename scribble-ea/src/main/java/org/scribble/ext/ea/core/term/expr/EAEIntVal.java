package org.scribble.ext.ea.core.term.expr;

import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVIntType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

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
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        return Either.right(Pair.of(
                EAVIntType.INT,
                new Tree<>("[TV-Unit] " + toTypeJudgeString(gamma, EAVIntType.INT))
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
    public boolean isValue() {
        return true;
    }

    /*@Override
    public boolean isGround() {
        return true;
    }*/

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
