package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.util.Pair;

import java.util.*;

public class EAMApp implements EAComp {

    // !!! vals -- use let for computation
    @NotNull
    public final EAExpr left;  // Not just rec/lam, could be a var
    @NotNull
    public final EAExpr right;

    public EAMApp(@NotNull EAExpr left, @NotNull EAExpr right) {
        this.left = left;
        this.right = right;
    }

    // !!! FIXME relocate below two

    // S^?
    public static boolean isInType(EALType t) {
        return t instanceof EALInType || t.unfoldAllOnce() instanceof EALInType;
    }

    @Override
    public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
        EAVType ltype = this.left.type(gamma);
        if (!(ltype instanceof EAVFuncType)) {
            throw new RuntimeException("Expected function type, not: "
                    + this.left + " : " + ltype + "\n" + gamma);
        }
        EAVFuncType ftype = (EAVFuncType) ltype;
        /*if (!ftype.S.equals(pre)) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + ftype.S + ", required=" + pre);
        }*/
        EALType.subtype(ftype.S, pre);
        EAVType rtype = this.right.type(gamma);
        if (!rtype.equals(ftype.A)) {
            throw new RuntimeException("Incompatible arg type:\n"
                    + "\tfound=" + rtype + ", required=" + ftype.A);
        }
        return new Pair<>(ftype.B, ftype.T);
    }

    @Override
    public EALType infer(GammaState gamma) {
        EAVType ftype = this.left.type(gamma);
        if (!(ftype instanceof EAVFuncType)) {
            throw new RuntimeException("Couldn't infer type: " + ftype);
        }
        return ((EAVFuncType) ftype).S;  // infer yields the I/O to be done
    }

    @Override
    public boolean canBeta() {
        return this.left instanceof EAERec  // TODO lambda
                //&& this.left.isGround() && this.right.isGround();  // FIXME separate isValue (for canBeta) from isGround
                && this.left.isValue() && this.right.isValue();
    }

    @Override
    public EAComp beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.left instanceof EAERec) {
            EAERec rec = (EAERec) this.left;
            return rec.body.fsubs(Map.of(rec.f, rec))
                    .subs(Map.of(rec.var, this.right));
        } else {
            throw new RuntimeException("TODO: " + this);
        }
    }

    @Override
    public EAComp getConfigRedexCandidate() {
        return this;
    }

    @Override
    public EAComp configReduce() {
        return beta();
    }

    /* Aux */

    @Override
    public EAMApp subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAExpr left = this.left.subs(m);
        EAExpr right = this.right.subs(m);
        return EATermFactory.factory.app(left, right);
    }

    @Override
    public EAMApp fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        EAExpr left = this.left.fsubs(m);
        EAExpr right = this.right.fsubs(m);
        return EATermFactory.factory.app(left, right);
    }

    @Override
    public EAComp recon(@NotNull EAComp old, EAComp neww) {
        //return this;  // XXX ?
        throw new RuntimeException("Needed? " + this);
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = new HashSet<>();
        res.addAll(this.left.getFreeVars());
        res.addAll(this.right.getFreeVars());
        return res;
    }

    /*@Override
    public boolean isGround() {
        return this.left.isGround() && this.right.isGround();
    }*/

    @Override
    public String toString() {
        return "[" + this.left + " " + this.right + "]";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMApp them = (EAMApp) o;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMApp;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.APP;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }
}
