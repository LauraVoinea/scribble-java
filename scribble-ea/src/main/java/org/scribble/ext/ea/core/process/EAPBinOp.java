package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAIntType;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EAPBinOp implements EAPVal {

    @NotNull
    public final EAPOp op;
    @NotNull
    public final EAPVal left;
    @NotNull
    public final EAPVal right;

    public EAPBinOp(@NotNull EAPOp op, @NotNull EAPVal left, @NotNull EAPVal right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }

    /*// S^?
    public static boolean isInType(EALType t) {
        return t instanceof EALInType || t.unfoldAllOnce() instanceof EALInType;
    }*/

    /*// found is expr, required is usually pre
    public static void subtype(EALType found, EALType required) {
        if (!found.equals(required) && !found.unfoldAllOnce().equals(required.unfoldAllOnce())) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + found + ", required=" + required);
        }
    }*/

    @Override
    public EAValType type(Gamma gamma) {
        if (this.op.equals(EAPOp.PLUS)) {
            EAValType ltype = this.left.type(gamma);
            if (!(ltype.equals(EAIntType.INT))) {
                throw new RuntimeException("Expected Int type, not: "
                        + this.left + " : " + ltype + "\n" + gamma);
            }
        /*EAFuncType ftype = (EAFuncType) ltype;
        /*if (!ftype.S.equals(pre)) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + ftype.S + ", required=" + pre);
        }* /
        subtype(ftype.S, pre);*/
            EAValType rtype = this.right.type(gamma);
            if (!(rtype.equals(EAIntType.INT))) {
                throw new RuntimeException("Incompatible arg type:\n"
                        + "\tfound=" + rtype + ", required=" + EAIntType.INT);
            }
            return EAIntType.INT;
        }
        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public EAPVal subs(Map<EAPVar, EAPVal> m) {
        return EAPFactory.factory.binop(this.op, this.left.subs(m), this.right.subs(m));
    }

    @Override
    public EAPVal fsubs(Map<EAPFuncName, EAPRec> m) {
        return EAPFactory.factory.binop(this.op, this.left.fsubs(m), this.right.fsubs(m));
    }

    /*@Override
    public EALType infer(Gamma gamma) {
        return EALEndType.END;  // No need to do `cat`, left/right are vals (not exprs)
    }*/

    @Override
    public boolean canBeta() {
        if (this.op.equals(EAPOp.PLUS)) {
            return (this.left instanceof EAPIntVal) && (this.right instanceof EAPIntVal);
        }
        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public EAPVal beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        EAPIntVal left = (EAPIntVal) this.left;
        EAPIntVal right = (EAPIntVal) this.right;
        return EAPFactory.factory.intt(left.val + right.val);
    }

    /* Aux */

    /*@Override
    public EAPBExpr recon(@NotNull EAPBExpr old, EAPBExpr neww) {
        return EAPFactory.factory.binop(this.op, thisleft + right.val);
    }*/

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = new HashSet<>();
        res.addAll(this.left.getFreeVars());
        res.addAll(this.right.getFreeVars());
        return res;
    }

    @Override
    public boolean isGround() {
        return this.left.isGround() && this.right.isGround();
        //return false;
    }

    /*@Override
    public EAPBExpr getFoo() {
        return this;
    }

    @Override
    public EAPBExpr foo() {
        return beta();
    }*/

    @Override
    public String toString() {
        return this.left + " " + this.op + " " + this.right;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPBinOp them = (EAPBinOp) o;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPBinOp;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.BIN_OP;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }
}
