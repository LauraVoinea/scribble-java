package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
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

// V op W -- V/W should be c/x (not, e.g., funcs)
// cf. EAMApp V W -- 1+2 vs. ((plus) 1) 2)
public class EAMBinOp implements EAComp {

    @NotNull public final EAOp op;
    @NotNull public final EAExpr left;
    @NotNull public final EAExpr right;

    public EAMBinOp(EAOp op, EAExpr left, EAExpr right) {
        this.op = op;
        this.left = left;
        this.right = right;
    }

    /* ... */

    @Override
    public EALType infer(GammaState gamma) {
        return EALEndType.END;  // No need to do `cat`, left/right are vals (not comps)
    }

    @Override
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(
            GammaState gamma, EALType pre) {

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
                return Either.right(Pair.of(
                        Pair.of(EAVIntType.INT, pre),
                        Tree.of(
                                "[..binop..]" + toTypeJudgeString(gamma, pre, EAVIntType.INT, pre),
                                List.of(p_l.right, p_r.right))
                ));
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
                return Either.right(Pair.of(
                        Pair.of(EAVBoolType.BOOL, pre),
                        Tree.of(
                                "[..binop..] " + toTypeJudgeString(gamma, pre, EAVBoolType.BOOL, pre),
                                List.of(p_l.right, p_r.right))
                ));
            }

            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

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

    // V op W ->_M return (eval(op, V, W))
    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        if (!canBeta()) {  // ...testing
            throw new RuntimeException("Stuck: " + this);
        }
        switch (this.op) {
            case PLUS: {
                EAEIntVal left = (EAEIntVal) this.left;
                EAEIntVal right = (EAEIntVal) this.right;
                EAMReturn res = EATermFactory.factory.returnn(
                        EATermFactory.factory.intt(left.val + right.val));
                return Either.right(Pair.of(res, Tree.of(
                        toBetaJudgeString("[..binop[+]..]", this, res))
                ));
            }
            case LT: {
                EAEIntVal left = (EAEIntVal) this.left;
                EAEIntVal right = (EAEIntVal) this.right;
                EAMReturn res = EATermFactory.factory.returnn(
                        EATermFactory.factory.bool(left.val < right.val));
                return Either.right(Pair.of(res, Tree.of(
                        toBetaJudgeString("[..binop[<]..]", this, res))
                ));
            }
            default:
                throw new RuntimeException("TODO: " + this);
        }
    }

    /* ... */

    @Override
    public EAComp getStepSubexprE() {
        return this;
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> contextStepE() {
        return beta().mapRight(x -> Pair.of(
                x.left,
                x.right)  // No E-Ctx, only Let is context (FG-CBV); Lift done by EACActor
        );
    }

    /* Aux */

    @Override
    public EAComp subs(Map<EAEVar, EAExpr> m) {
        return EATermFactory.factory.binop(this.op, this.left.subs(m),
                this.right.subs(m));
    }

    @Override
    public EAComp fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        return EATermFactory.factory.binop(this.op, this.left.fsubs(m),
                this.right.fsubs(m));
    }

    @Override
    public EAComp recon(@NotNull EAComp old, @NotNull EAComp neww) {
        //return EAPFactory.factory.binop(this.op, thisleft + right.val);
        throw new RuntimeException("Deprecated: " + this);
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
    public String toString() {
        return this.left + " " + this.op + " " + this.right;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMBinOp them = (EAMBinOp) o;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMBinOp;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.BIN_OP;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }
}
