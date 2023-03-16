package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAFuncType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.*;

public class EAPApp implements EAPExpr {

    // !!! vals -- use let for computation
    @NotNull
    public final EAPVal left;  // Not just rec/lam, could be a var
    @NotNull
    public final EAPVal right;

    public EAPApp(@NotNull EAPVal left, @NotNull EAPVal right) {
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
    public EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre) {
        EAValType ltype = this.left.type(gamma);
        if (!(ltype instanceof EAFuncType)) {
            throw new RuntimeException("Expected function type, not: "
                    + this.left + " : " + ltype + "\n" + gamma);
        }
        EAFuncType ftype = (EAFuncType) ltype;
        /*if (!ftype.S.equals(pre)) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + ftype.S + ", required=" + pre);
        }*/
        subtype(ftype.S, pre);
        EAValType rtype = this.right.type(gamma);
        if (!rtype.equals(ftype.A)) {
            throw new RuntimeException("Incompatible arg type:\n"
                    + "\tfound=" + rtype + ", required=" + ftype.A);
        }
        return new EAPPair<>(ftype.B, ftype.T);
    }

    @Override
    public EALType infer(Gamma gamma) {
        EAValType ftype = this.left.type(gamma);
        if (!(ftype instanceof EAFuncType)) {
            throw new RuntimeException("Couldn't infer type: " + ftype);
        }
        return ((EAFuncType) ftype).S;  // infer yields the I/O to be done
    }

    @Override
    public boolean canBeta() {
        return this.left instanceof EAPRec  // TODO lambda
                && this.left.isGround() && this.right.isGround();  // Redundant?
    }

    @Override
    public EAPExpr beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.left instanceof EAPRec) {
            EAPRec rec = (EAPRec) this.left;
            return rec.body.fsubs(Map.of(rec.f, rec))
                    .subs(Map.of(rec.var, this.right));
        } else {
            throw new RuntimeException("TODO: " + this);
        }
    }

    /* Aux */

    @Override
    public EAPApp subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal left = this.left.subs(m);
        EAPVal right = this.right.subs(m);
        return EAPFactory.factory.app(left, right);
    }

    @Override
    public EAPApp fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        EAPVal left = this.left.fsubs(m);
        EAPVal right = this.right.fsubs(m);
        return EAPFactory.factory.app(left, right);
    }

    @Override
    public EAPExpr recon(@NotNull EAPExpr old, EAPExpr neww) {
        return this;
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = new HashSet<>();
        res.addAll(this.left.getFreeVars());
        res.addAll(this.right.getFreeVars());
        return res;
    }

    @Override
    public boolean isGround(Set<EAPFuncName> fnames) {
        return this.left.isGround(fnames) && this.right.isGround(fnames);  // !!! bad naming
    }

    @Override
    public EAPExpr getFoo() {
        return this;
    }

    @Override
    public EAPExpr foo() {
        return beta();
    }

    @Override
    public String toString() {
        return this.left + "(" + this.right + ")";
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
        int hash = EAPTerm.APP;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }
}
