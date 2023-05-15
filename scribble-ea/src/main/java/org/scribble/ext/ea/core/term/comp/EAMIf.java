package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.*;

public class EAMIf implements EAComp {

    @NotNull
    public final EAExpr cond;
    @NotNull
    public final EAComp then;
    @NotNull
    public final EAComp elsee;

    public EAMIf(@NotNull EAExpr cond, @NotNull EAComp then, @NotNull EAComp elsee) {
        this.cond = cond;
        this.then = then;
        this.elsee = elsee;
    }

    @Override
    //public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {
        //Pair<EAVType, EALType> ttype = this.then.type(gamma, pre);
        //Pair<EAVType, EALType> etype = this.elsee.type(gamma, pre);

        // FIXME: type cond

        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> t_l = this.then.type(gamma, pre);
        if (t_l.isLeft()) {
            return Either.left(t_l.getLeft());
        }
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> t_r = this.elsee.type(gamma, pre);
        if (t_r.isLeft()) {
            return Either.left(t_r.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> pp_l = t_l.getRight();
        Pair<Pair<EAVType, EALType>, Tree<String>> pp_r = t_r.getRight();
        Pair<EAVType, EALType> ttype = pp_l.left;
        Pair<EAVType, EALType> etype = pp_r.left;
        //////subtype(ftype.S, pre);
        ////if (!ttype.equals(etype)) {
        Optional<EAVType> u = EAVType.unify(ttype.left, etype.left);
        if (!u.isPresent() || !ttype.right.equals(etype.right)) {
            //throw new RuntimeException("Incompatible branches:\n\tfound=" + ttype + ", required=" + etype);
            return Either.left(new Exception("Incompatible branches:\n\tfound=" + ttype + ", required=" + etype));
        }
        //return new Pair<>(u.get(), ttype.right);
        EAVType B = u.get();
        return Either.right(Pair.of(
                Pair.of(B, ttype.right),
                new Tree<>("[T-If] " + toTypeJudgeString(gamma, pre, B, ttype.right),
                        List.of(pp_l.right, pp_r.right))
        ));
    }

    @Override
    public EALType infer(GammaState gamma) {
        /*EAValType ftype = this.left.type(gamma);
        if (!(ftype instanceof EAFuncType)) {
            throw new RuntimeException("Couldn't infer type: " + ftype);
        }
        return ((EAFuncType) ftype).S;  // infer yields the I/O to be done*/
        throw new RuntimeException("Not used? " + this);
    }

    @Override
    public boolean canBeta() {
        return this.cond.canEval() || this.cond instanceof EAEBoolVal;
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        if (!canBeta()) {  // ...testing
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.cond.isValue()) {
            if (!(this.cond instanceof EAEBoolVal)) {
                return Either.left(new Exception("Expected Bool condition, not: " + this.cond));
            }
            EAComp res = ((EAEBoolVal) this.cond).val ? this.then : this.elsee;
            return Either.right(Pair.of(res, Tree.of(
                    toBetaJudgeString("[..B-If..]", this, res))
            ));
        }
        Either<Exception, Pair<EAExpr, Tree<String>>> eval = this.cond.eval();
        return eval.mapRight(x -> {
            EAMIf res = EATermFactory.factory.iff(x.left, this.then, this.elsee);
            return Pair.of(res, Tree.of(
                    toBetaJudgeString("[..B-Ctx..]", this, res),
                    x.right)  // XXX FIXME refactor binop to Comp, only Let should be context  // !!! deprecate (only Let should be E-Context)
            );
        });
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
    public EAMIf subs(@NotNull Map<EAEVar, EAExpr> m) {
        return EATermFactory.factory.iff(
                this.cond.subs(m), this.then.subs(m), this.elsee.subs(m));
    }

    @Override
    public EAMIf fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        return EATermFactory.factory.iff(
                this.cond.fsubs(m), this.then.fsubs(m), this.elsee.fsubs(m));
    }

    @Override
    public EAComp recon(@NotNull EAComp old, EAComp neww) {
        throw new RuntimeException("Needed? " + this);
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = new HashSet<>();
        res.addAll(this.cond.getFreeVars());
        res.addAll(this.then.getFreeVars());
        res.addAll(this.elsee.getFreeVars());
        return res;
    }

    /*@Override
    public boolean isGround() {
        return this.cond.isGround() && this.then.isGround() && this.elsee.isGround();  // !!! bad naming
    }*/

    @Override
    public String toString() {
        return "if " + this.cond + " then " + this.then + " else " + this.elsee;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMIf them = (EAMIf) o;
        return them.canEquals(this)
                && this.cond.equals(them.cond)
                && this.then.equals(them.then)
                && this.elsee.equals(them.elsee);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMIf;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.IF;
        hash = 31 * hash + this.cond.hashCode();
        hash = 31 * hash + this.then.hashCode();
        hash = 31 * hash + this.elsee.hashCode();
        return hash;
    }
}
