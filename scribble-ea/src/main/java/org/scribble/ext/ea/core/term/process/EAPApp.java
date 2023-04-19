package org.scribble.ext.ea.core.term.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.*;

public class EAPApp implements EAComp {

    // !!! vals -- use let for computation
    @NotNull
    public final EAExpr left;  // Not just rec/lam, could be a var
    @NotNull
    public final EAExpr right;

    public EAPApp(@NotNull EAExpr left, @NotNull EAExpr right) {
        this.left = left;
        this.right = right;
    }

    // !!! FIXME relocate below two

    // S^?
    public static boolean isInType(EALType t) {
        return t instanceof EALInType || t.unfoldAllOnce() instanceof EALInType;
    }

    // found is expr, required is usually pre
    public static void subtype(EALType found, EALType required) {
        if (!found.equals(required) && !found.unfoldAllOnce().equals(required.unfoldAllOnce())) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + found + ", required=" + required);
        }
    }

    @Override
    public EAPPair<EAVType, EALType> type(Gamma gamma, EALType pre) {
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
        subtype(ftype.S, pre);
        EAVType rtype = this.right.type(gamma);
        if (!rtype.equals(ftype.A)) {
            throw new RuntimeException("Incompatible arg type:\n"
                    + "\tfound=" + rtype + ", required=" + ftype.A);
        }
        return new EAPPair<>(ftype.B, ftype.T);
    }

    @Override
    public EALType infer(Gamma gamma) {
        EAVType ftype = this.left.type(gamma);
        if (!(ftype instanceof EAVFuncType)) {
            throw new RuntimeException("Couldn't infer type: " + ftype);
        }
        return ((EAVFuncType) ftype).S;  // infer yields the I/O to be done
    }

    @Override
    public boolean canBeta() {
        return this.left instanceof EAERec  // TODO lambda
                && this.left.isGround() && this.right.isGround();  // FIXME separate isValue (for canBeta) from isGround
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

    /* Aux */

    @Override
    public EAPApp subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAExpr left = this.left.subs(m);
        EAExpr right = this.right.subs(m);
        return EATermFactory.factory.app(left, right);
    }

    @Override
    public EAPApp fsubs(@NotNull Map<EAFuncName, EAERec> m) {
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

    @Override
    public boolean isGround() {
        return this.left.isGround() && this.right.isGround();
    }

    @Override
    public EAComp getConfigRedexCandidate() {
        return this;
    }

    @Override
    public EAComp configStep() {
        return beta();
    }

    @Override
    public String toString() {
        return "[" + this.left + " " + this.right + "]";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPApp them = (EAPApp) o;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPApp;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.APP;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }
}
