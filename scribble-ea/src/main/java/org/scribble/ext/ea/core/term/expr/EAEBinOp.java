package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVBoolType;
import org.scribble.ext.ea.core.type.value.EAVIntType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EAEBinOp implements EAExpr {

    @NotNull
    public final EAEOp op;
    @NotNull
    public final EAExpr left;
    @NotNull
    public final EAExpr right;

    public EAEBinOp(@NotNull EAEOp op, @NotNull EAExpr left, @NotNull EAExpr right) {
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
    public EAVType infer() {
        switch (this.op) {
            case PLUS:
                return EAVIntType.INT;
            case LT:
                return EAVBoolType.BOOL;
            default:
                throw new RuntimeException("Unknown op: " + this.op);
        }
    }

    @Override
    public EAVType type(Gamma gamma) {
        switch (this.op) {
            case PLUS: {
                EAVType ltype = this.left.type(gamma);
                if (!(ltype.equals(EAVIntType.INT))) {
                    throw new RuntimeException("Expected Int type, not: "
                            + this.left + " : " + ltype + "\n" + gamma);
                }
        /*EAFuncType ftype = (EAFuncType) ltype;
        /*if (!ftype.S.equals(pre)) {
            throw new RuntimeException("Incompatible pre type:\n"
                    + "\tfound=" + ftype.S + ", required=" + pre);
        }* /
        subtype(ftype.S, pre);*/
                EAVType rtype = this.right.type(gamma);
                if (!(rtype.equals(EAVIntType.INT))) {
                    throw new RuntimeException("Incompatible arg type:\n"
                            + "\tfound=" + rtype + ", required=" + EAVIntType.INT);
                }
                return EAVIntType.INT;
            }
            case LT: {
                EAVType ltype = this.left.type(gamma);
                if (!(ltype.equals(EAVIntType.INT))) {
                    throw new RuntimeException("Expected Int type, not: "
                            + this.left + " : " + ltype + "\n" + gamma);
                }
                EAVType rtype = this.right.type(gamma);
                if (!(rtype.equals(EAVIntType.INT))) {
                    throw new RuntimeException("Incompatible arg type:\n"
                            + "\tfound=" + rtype + ", required=" + EAVIntType.INT);
                }
                return EAVBoolType.BOOL;
            }
            default:
                throw new RuntimeException("TODO: " + this);

        }
    }

    @Override
    public EAExpr subs(Map<EAEVar, EAExpr> m) {
        return EATermFactory.factory.binop(this.op, this.left.subs(m), this.right.subs(m));
    }

    @Override
    public EAExpr fsubs(Map<EAFuncName, EAERec> m) {
        return EATermFactory.factory.binop(this.op, this.left.fsubs(m), this.right.fsubs(m));
    }

    /*@Override
    public EALType infer(Gamma gamma) {
        return EALEndType.END;  // No need to do `cat`, left/right are vals (not exprs)
    }*/

    @Override
    public boolean canBeta() {
        switch (this.op) {
            case PLUS:
            case LT:
                return (this.left instanceof EAEIntVal) && (this.right instanceof EAEIntVal);
            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

    @Override
    public EAExpr beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        switch (this.op) {
            case PLUS: {
                EAEIntVal left = (EAEIntVal) this.left;
                EAEIntVal right = (EAEIntVal) this.right;
                return EATermFactory.factory.intt(left.val + right.val);
            }
            case LT: {
                EAEIntVal left = (EAEIntVal) this.left;
                EAEIntVal right = (EAEIntVal) this.right;
                return EATermFactory.factory.bool(left.val < right.val);
            }
            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

    /* Aux */

    /*@Override
    public EAPBExpr recon(@NotNull EAPBExpr old, EAPBExpr neww) {
        return EAPFactory.factory.binop(this.op, thisleft + right.val);
    }*/

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
        EAEBinOp them = (EAEBinOp) o;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEBinOp;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.BIN_OP;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }
}
