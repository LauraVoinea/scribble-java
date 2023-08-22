package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVHandlersType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

// rec should be used with lam, e.g., rec f(x). \x. M  with f(..) inside M
// ...here the lam is "inlined" into the rec
public class EAERec implements EAExpr {

    @NotNull
    public final EAEFuncName f;
    @NotNull
    public final EAEVar var;  // hardcoded single param
    @NotNull
    public final EAVType varType;  // A -- hardcode single param
    @NotNull
    public final EAComp body;

    @NotNull
    public final EALType S;
    @NotNull
    public final EALType T;
    @NotNull
    public final EAVType B;

    //public EAPLet(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAERec(@NotNull EAEFuncName f, @NotNull EAEVar var,
                  @NotNull EAVType varType, @NotNull EAComp body,
                  @NotNull EALType S, @NotNull EALType T, @NotNull EAVType B) {
        this.f = f;
        this.var = var;
        this.varType = varType;
        this.body = body;

        this.S = S;
        this.T = T;
        this.B = B;
    }

    @Override
    public EAVFuncType infer() {
        return EATypeFactory.factory.val.func(this.varType, this.S, this.T, this.B);
    }

    @Override
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.gamma.map);
        tmp.put(this.var, this.varType);
        LinkedHashMap<EAEFuncName, EAVFuncType> ftmp = new LinkedHashMap<>(gamma.gamma.fmap);
        EAVFuncType ftype = EATypeFactory.factory.val.func(this.varType, this.S, this.T, this.B);
        ftmp.put(this.f, ftype);
        GammaState gamma1 = new GammaState(tmp, ftmp, gamma.svarType);
        //Pair<EAVType, EALType> res = this.body.type(gamma1, this.S);
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> res1 = this.body.type(gamma1, this.S);
        if (res1.isLeft()) {
            return Either.left(res1.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> pp = res1.getRight();
        Pair<EAVType, EALType> res = pp.left;

        //Pair<EAVType, EALType> target = new Pair<>(this.B, this.S);
        Pair<EAVType, EALType> target = new Pair<>(this.B, this.T);

        // lambda return typing (TV-Lam, B/T) "inlined" into this rec (TV-Rec)
        /*if (!res.equals(target)) {
            //throw new RuntimeException("Typing error:\n\t" + res + "\n\t" + target);
            return Either.left(new Exception("Typing error:\n\t" + this.body + " : " + res + "\n\t" + "result annots = " + target + "\n\t" + this));
        }*/
        if (!res.right.equals(target.right)) {
            return Either.left(new Exception("Session typing error:\n\t" + this.body + " : " + res + "\n\t" + "result annots = " + target + "\n\t" + this));
        }
        if (target.left instanceof EAVHandlersType) {
            if (!(res.left instanceof EAVHandlersType)) {
                return Either.left(new Exception("Value typing error:\n\t" + this.body + " : " + res.left + "\n\t" + "result annot = " + target.left + "\n\t" + this));
            }
            EALType tt = ((EAVHandlersType) target.left).S;
            EALType rr = ((EAVHandlersType) res.left).S;
            EALType.equalSubFold(rr, tt);
        } else if (!res.left.equals(target.left)) {
            return Either.left(new Exception("Value typing error:\n\t" + this.body + " : " + res.left + "\n\t" + "result annot = " + target.left + "\n\t" + this));
        }

        return Either.right(Pair.of(ftype,
                new Tree<>("[..TV-Rec/TV-Lam..] " + toTypeJudgeString(gamma, ftype))));  // ...discarded pp.right
    }

    @Override
    public Either<Exception, Pair<EAExpr, Tree<String>>> eval() {
        return Either.left(newStuck());
    }

    /* Aux */

    @Override
    public EAERec subs(@NotNull Map<EAEVar, EAExpr> m) {
        Map<EAEVar, EAExpr> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAComp body1 = body.subs(m1);
        return EATermFactory.factory.rec(this.f, this.var, this.varType, body1,
                this.S, this.T, this.B);
    }

    @Override
    public EAExpr fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        Map<EAEFuncName, EAERec> m1 = new HashMap<>(m);
        m1.remove(this.f);
        EAComp body1 = body.fsubs(m1);
        return EATermFactory.factory.rec(this.f, this.var, this.varType, body1,
                this.S, this.T, this.B);
    }

    // CHECKME: how about free f names?
    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = this.body.getFreeVars();
        res.remove(this.var);
        return res;
    }

    @Override
    public boolean isValue() {
        //return getFreeVars().stream().allMatch(x -> x.equals(this.var));
        return isGround();
    }

    /*@Override
    public boolean isGround() {
        return this.body.isGround();
    }*/

    @Override
    public String toString() {
        return "rec " + this.f + " "
                + ConsoleColors.toAnnotString("{" + this.S + "}")
                + " (" + this.var
                + ConsoleColors.toAnnotString(": " + this.varType)
                + "): "
                + ConsoleColors.toAnnotString(this.B.toString() + " {" + this.T + "}")
                + " . " + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }
        EAERec them = (EAERec) o;
        return them.canEquals(this)
                && this.f.equals(them.f)
                && this.var.equals(them.var)
                && this.varType.equals(them.varType)
                && this.body.equals(them.body)
                && this.S.equals(them.S)
                && this.T.equals(them.T)
                && this.B.equals(them.B);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAERec;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.REC;
        hash = 31 * hash + this.f.hashCode();
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.varType.hashCode();
        hash = 31 * hash + this.body.hashCode();
        hash = 31 * hash + this.S.hashCode();
        hash = 31 * hash + this.T.hashCode();
        hash = 31 * hash + this.B.hashCode();
        return hash;
    }
}
