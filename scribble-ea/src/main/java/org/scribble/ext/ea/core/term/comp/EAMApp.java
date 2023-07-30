package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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

    @Override
    //public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(
            GammaState gamma, EALType pre) {
        //EAVType ltype = this.left.type(gamma);
        Either<Exception, Pair<EAVType, Tree<String>>> t_l = this.left.type(gamma);
        if (t_l.isLeft()) {
            return Either.left(t_l.getLeft());
        }
        Pair<EAVType, Tree<String>> p_l = t_l.getRight();
        EAVType ltype = p_l.left;
        if (!(ltype instanceof EAVFuncType)) {
            //throw new RuntimeException("Expected function type, not: " + this.left + " : " + ltype + "\n" + gamma);
            return Either.left(new Exception("Expected function type, not: " + this.left + " : " + ltype + "\n" + gamma));
        }
        EAVFuncType ftype = (EAVFuncType) ltype;
//        /*if (!ftype.S.equals(pre)) {
//            throw new RuntimeException("Incompatible pre type:\n"
//                    + "\tfound=" + ftype.S + ", required=" + pre);
//        }*/
        System.out.println("------- " + this);

        EALType.subtype(ftype.S, pre);  // (probably) need "subtype" for run-time type pres  // TODO use Either
        /*if (!(ftype.S.equals(pre))) {
            return Either.left(new Exception("Incompatible pre type: " + pre + ", " + ftype.S + "\n\t" + this));
        }*/

        //EAVType rtype = this.right.type(gamma);
        Either<Exception, Pair<EAVType, Tree<String>>> t_r = this.right.type(gamma);
        if (t_r.isLeft()) {
            return Either.left(t_r.getLeft());
        }
        Pair<EAVType, Tree<String>> p_r = t_r.getRight();
        EAVType rtype = p_r.left;
        if (!rtype.equals(ftype.A)) {
            //throw new RuntimeException("Incompatible arg type:\n\tfound=" + rtype + ", required=" + ftype.A);
            return Either.left(new Exception("Incompatible arg type:\n\tfound=" + rtype + ", required=" + ftype.A));
        }
        //return new Pair<>(ftype.B, ftype.T);
        return Either.right(Pair.of(
                Pair.of(ftype.B, ftype.T),
                Tree.of("[T-App] " + toTypeJudgeString(gamma, pre, ftype.B, ftype.T),
                        List.of(p_l.right, p_r.right))
        ));
    }

    @Override
    public EALType infer(GammaState gamma) {
        //EAVType ftype = this.left.type(gamma);
        Either<Exception, Pair<EAVType, Tree<String>>> t = this.left.type(gamma);
        if (t.isLeft()) {
            //return Either.left(new Exception(t_l.getLeft().get()));
            throw new RuntimeException("Couldn't infer type: " + this);  // TODO use Either
        }
        Pair<EAVType, Tree<String>> p = t.getRight();
        EAVType ftype = p.left;
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
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        if (!canBeta()) {  // ...testing
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.left.isValue()) {
            if (!(this.left instanceof EAERec)) {  // Currently only Rec (no Lam)
                return Either.left(new Exception("Expected rec-val, not: " + this.left));
            }
            EAERec rec = (EAERec) this.left;
            EAComp res = rec.body.fsubs(Map.of(rec.f, rec)).subs(Map.of(rec.var, this.right));
            return Either.right(Pair.of(res, Tree.of(
                    toBetaJudgeString("[B-App-Rec]", this, res))
            ));
        }
        return Either.left(new Exception("Stuck: " + this));
    }

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
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }
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
