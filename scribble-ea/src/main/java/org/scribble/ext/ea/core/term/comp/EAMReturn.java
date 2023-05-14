package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.List;
import java.util.Map;
import java.util.Set;

// !!! encodable using suspend? apart from "GC" flavour
public class EAMReturn implements EAComp {

    @NotNull
    public final EAExpr val;  // value, not expr

    public EAMReturn(EAExpr val) {
        this.val = val;
    }

    @Override
    //public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {
        /*if (!pre.equals(end)) {  // !!! return is value/term typing wrapper, not (session) control flow
            throw new RuntimeException("Expected end type: " + pre);
        }*/
        //EAVType t = this.val.type(gamma);
        Either<Exception, Pair<EAVType, Tree<String>>> t = this.val.type(gamma);
        ////return new Pair<>(t, end);
        //return new Pair<>(t, pre);
        return t.mapRight(x -> Pair.of(
                Pair.of(x.left, pre),
                new Tree<>("[T-Return] " + toTypeJudgeString(gamma, pre, x.left, pre),
                        List.of(x.right))));
    }

    @Override
    public EALEndType infer(GammaState gamma) {
        return EALEndType.END;  // !!! (potential) placeholder
    }

    @Override
    public boolean canBeta() {
        //return false;
        return this.val.canEval();
    }

    // Currently hacked to allow eval of, e.g., return 2+3 -- cf. (plus 2) 3 as an M
    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        Either<Exception, Pair<EAExpr, Tree<String>>> eval = this.val.eval();
        return eval.mapRight(x -> Pair.of(
                EATermFactory.factory.returnn(x.left),
                new Tree<>("[..B-Ctx-Ret..]", x.right)
        ));
    }

    @Override
    public EAComp getConfigRedexCandidate() {
        return this;  // basically for top-level return -- let-init return detected by EAMLet
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> configReduce() {
        //return EATermFactory.factory.returnn(this.val.eval());
        return this.val.eval().mapRight(x -> Pair.of(
                EATermFactory.factory.returnn(x.left),
                new Tree<>("[..E-Lift-Ret..]", x.right)
        ));
    }

    /* Aux */

    @Override
    public EAMReturn subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAExpr val1 = this.val.subs(m);
        return EATermFactory.factory.returnn(val1);
    }

    @Override
    public EAMReturn fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        EAExpr val1 = this.val.fsubs(m);
        return EATermFactory.factory.returnn(val1);
    }

    @Override
    public EAMReturn recon(@NotNull EAComp old, EAComp neww) {
        return this;
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    /*@Override
    public boolean isGround() {
        return this.val.isGround();
    }*/

    /*@Override
    public boolean canFoo() {
        return false;  // !!! look for top-level return () in config -- let x <= return V handled by let
    }*/

    @Override
    public boolean isGroundValueReturn() {
        return this.val.isValue();
    }

    @Override
    public String toString() {
        return "return " + this.val;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMReturn eaVar = (EAMReturn) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMReturn;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.RETURN;
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
