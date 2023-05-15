package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.*;

public class EAMLet implements EAComp {

    @NotNull
    public final EAEVar var;
    @NotNull
    public final EAVType varType;  // vars x have ValTypes A -- !!! added type annot
    //@NotNull public final EAPExpr init;  // !!! value?  not expr
    @NotNull
    public final EAComp init;
    @NotNull
    public final EAComp body;

    //public EAPLet(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAMLet(@NotNull EAEVar var, @NotNull EAVType varType,
                  @NotNull EAComp init, @NotNull EAComp body) {
        this.var = var;
        this.varType = varType;
        this.init = init;
        this.body = body;
    }

    @Override
    //public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {
        //Pair<EAVType, EALType> p1 = this.init.type(gamma, pre);
        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> t = this.init.type(gamma, pre);
        if (t.isLeft()) {
            return t;
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> pp = t.getRight();
        Pair<EAVType, EALType> p1 = pp.left;
        if (!this.varType.equals(p1.left)) {
            //throw new RuntimeException("Bad type annotation: " + this.varType + ", " + p1.left);
            return Either.left(new Exception("Bad type annotation: " + this.varType + ", " + p1.left));
        }
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.gamma.map);
        tmp.put(this.var, p1.left);
        GammaState gamma1 = new GammaState(tmp, new LinkedHashMap<>(gamma.gamma.fmap), gamma.svarType);
        return this.body.type(gamma1, p1.right).mapRight(x -> Pair.of(
                x.left,
                new Tree<>("[T-Let] " + toTypeJudgeString(gamma, pre, x.left.left, x.left.right),
                        List.of(pp.right, x.right))
        ));
    }

    @Override
    public EALType infer(GammaState gamma) {
        EALType i = this.init.infer(gamma);
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.gamma.map);
        tmp.put(this.var, this.varType);
        GammaState gamma1 = new GammaState(tmp, new LinkedHashMap<>(gamma.gamma.fmap), gamma.svarType);
        EALType b = this.body.infer(gamma1);
        return i.concat(b);
    }

    @Override
    public boolean canBeta() {
        return this.init.canBeta() || this.init.isGroundValueReturn();
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        if (!canBeta()) {  // ...testing
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.init.isGroundValueReturn()) {
            return Either.right(Pair.of(
                    this.body.subs(Map.of(this.var, ((EAMReturn) this.init).val)),
                    new Tree<>("[B-Let]")
            ));
        }
        Either<Exception, Pair<EAComp, Tree<String>>> beta = this.init.beta();
        return beta.mapRight(x -> Pair.of(
                EATermFactory.factory.let(this.var, this.varType, x.left, this.body),
                new Tree<>("[B-Ctx-Let]", x.right)
        ));
    }

    // foo (getConfigRedexCandidate) return corresponds with beta "subject"
    @Override
    public EAComp getStepSubexprE() {
        /*if (this.init instanceof EAMReturn //&& ((EAPReturn) this.init).val.isGround()
                && !((EAMReturn) this.init).val.canEval()) {*/
        if (this.init.isGroundValueReturn()) {
            return this;
        } else {
            return this.init.getStepSubexprE();
        }
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> contextStepE() {
        // Not just beta because, e.g., Send in init cannot beta (must configReduce)
        if (this.init.isGroundValueReturn()) {
            return beta().mapRight(x -> Pair.of(
                    x.left,
                    x.right)
            );
        }
        Either<Exception, Pair<EAComp, Tree<String>>> reduce = this.init.contextStepE();
        return reduce.mapRight(x -> Pair.of(
                EATermFactory.factory.let(this.var, this.varType, x.left, this.body),
                new Tree<>("[..E-Ctx-Let..]", x.right)
        ));
    }

    /* Aux */

    @Override
    public EAMLet subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAComp init1 = this.init.subs(m);
        Map<EAEVar, EAExpr> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAComp body1 = body.subs(m1);
        return EATermFactory.factory.let(this.var, this.varType, init1, body1);
    }

    @Override
    public EAMLet fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        EAComp init1 = this.init.fsubs(m);
        EAComp body1 = body.fsubs(m);
        return EATermFactory.factory.let(this.var, this.varType, init1, body1);
    }

    @Override
    public EAComp recon(@NotNull EAComp old, EAComp neww) {
        EAComp init1 = this.init.recon(old, neww);
        return EATermFactory.factory.let(this.var, this.varType, init1, this.body);  // !!! CHECKME: body unchanged
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = this.init.getFreeVars();
        res.addAll(this.body.getFreeVars());
        res.remove(this.var);
        return res;
    }

    /*@Override
    public boolean isGround() {
        //return this.init.isGround();  // !!! bad naming
        return getFreeVars().isEmpty();
    }*/

    @Override
    public String toString() {
        return "let " + this.var
                //+ ConsoleColors.BLACK_UNDERLINED + ":" + this.varType + ConsoleColors.RESET
                + ConsoleColors.toAnnotString(": " + this.varType)
                + " <= " + this.init + " in " + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMLet them = (EAMLet) o;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.varType.equals(them.varType)
                && this.init.equals(them.init)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMLet;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.LET;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.varType.hashCode();
        hash = 31 * hash + this.init.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }
}
