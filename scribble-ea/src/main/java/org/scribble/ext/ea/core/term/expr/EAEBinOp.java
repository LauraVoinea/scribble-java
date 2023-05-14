package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVBoolType;
import org.scribble.ext.ea.core.type.value.EAVIntType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class EAEBinOp implements EAExpr {

    @NotNull
    public final EAOp op;
    @NotNull
    public final EAExpr left;
    @NotNull
    public final EAExpr right;

    public EAEBinOp(@NotNull EAOp op, @NotNull EAExpr left, @NotNull EAExpr right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }

    /* ... */

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
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        Either<Exception, Pair<EAVType, Tree<String>>> t_l = this.left.type(gamma);
        if (t_l.isLeft()) {
            return Either.left(t_l.getLeft());
        }
        Pair<EAVType, Tree<String>> p_l = t_l.getRight();
        EAVType ltype = p_l.left;
        Either<Exception, Pair<EAVType, Tree<String>>> t_r = this.right.type(gamma);
        if (t_r.isLeft()) {
            return Either.left(t_r.getLeft());
        }
        Pair<EAVType, Tree<String>> p_r = t_r.getRight();
        EAVType rtype = p_r.left;

        switch (this.op) {
            case PLUS: {
                if (!(ltype.equals(EAVIntType.INT))) {
                    //throw new RuntimeException("Expected Int type, not: " + this.left + " : " + ltype + "\n" + gamma);
                    return Either.left(new Exception("Expected Int type, not: " + this.left + " : " + ltype + "\n" + gamma));
                }
                if (!(rtype.equals(EAVIntType.INT))) {
                    //throw new RuntimeException("Incompatible arg type:\n\tfound=" + rtype + ", required=" + EAVIntType.INT);
                    return Either.left(new Exception("Incompatible arg type:\n\tfound=" + rtype + ", required=" + EAVIntType.INT));
                }
                //return EAVIntType.INT;
                return Either.right(new Pair<>(
                        EAVIntType.INT,
                        new Tree<>("[..binop..]", List.of(p_l.right, p_r.right))));
            }
            case LT: {
                //EAVType ltype = this.left.type(gamma);
                if (!(ltype.equals(EAVIntType.INT))) {
                    //throw new RuntimeException("Expected Int type, not: " + this.left + " : " + ltype + "\n" + gamma);
                    return Either.left(new Exception("Expected Int type, not: " + this.left + " : " + ltype + "\n" + gamma));
                }
                if (!(rtype.equals(EAVIntType.INT))) {
                    //throw new RuntimeException("Incompatible arg type:\n\tfound=" + rtype + ", required=" + EAVIntType.INT);
                    return Either.left(new Exception(new Exception("Incompatible arg type:\n\tfound=" + rtype + ", required=" + EAVIntType.INT)));
                }
                //return EAVBoolType.BOOL;
                return Either.right(new Pair<>(
                        EAVBoolType.BOOL,
                        new Tree<>("[..binop..]", List.of(p_l.right, p_r.right))));
            }
            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

    /*@Override
    public EALType infer(Gamma gamma) {
        return EALEndType.END;  // No need to do `cat`, left/right are vals (not exprs)
    }*/

    @Override
    public boolean canEval() {
        switch (this.op) {
            case PLUS:
            case LT:
                return (this.left instanceof EAEIntVal) && (this.right instanceof EAEIntVal);
            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

    @Override
    public Either<Exception, Pair<EAExpr, Tree<String>>> eval() {
        if (!canEval()) {
            return Either.left(newStuck());
        }
        switch (this.op) {
            case PLUS: {
                EAEIntVal left = (EAEIntVal) this.left;
                EAEIntVal right = (EAEIntVal) this.right;
                return Either.right(Pair.of(
                        EATermFactory.factory.intt(left.val + right.val),
                        new Tree<>("..binop[+]..")
                ));
            }
            case LT: {
                EAEIntVal left = (EAEIntVal) this.left;
                EAEIntVal right = (EAEIntVal) this.right;
                return Either.right(Pair.of(
                        EATermFactory.factory.bool(left.val < right.val),
                        new Tree<>("..binop[<]..")
                ));
            }
            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

    /* Aux */

    @Override
    public EAExpr subs(Map<EAEVar, EAExpr> m) {
        return EATermFactory.factory.binop(this.op, this.left.subs(m), this.right.subs(m));
    }

    @Override
    public EAExpr fsubs(Map<EAEFuncName, EAERec> m) {
        return EATermFactory.factory.binop(this.op, this.left.fsubs(m), this.right.fsubs(m));
    }

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
    public boolean isValue() {
        return false;
    }

    /*@Override
    public boolean isGround() {
        return this.left.isGround() && this.right.isGround();
        //return false;
    }*/

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
