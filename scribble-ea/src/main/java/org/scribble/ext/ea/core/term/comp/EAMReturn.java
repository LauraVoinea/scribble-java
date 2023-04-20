package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.util.Pair;

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
    public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
        EALEndType end = EATypeFactory.factory.local.end();
        /*if (!pre.equals(end)) {  // !!! return is value/term typing wrapper, not (session) control flow
            throw new RuntimeException("Expected end type: " + pre);
        }*/
        EAVType t = this.val.type(gamma);
        //return new Pair<>(t, end);
        return new Pair<>(t, pre);
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

    @Override
    public EAComp beta() {
        //throw new RuntimeException("Stuck: " + this);
        //System.out.println("33333333: " + EATermFactory.factory.returnn(this.val.eval()));
        return EATermFactory.factory.returnn(this.val.eval());
    }

    // foo
    @Override
    public EAComp getConfigRedexCandidate() {
        //throw new RuntimeException("Shouldn't get here: " + this);
        return this;  // basically for top-level return -- let-init return detected by EAMLet
    }

    @Override
    public EAComp configReduce() {
        //throw new RuntimeException("Shouldn't get in here: " + this);
        return EATermFactory.factory.returnn(this.val.eval());
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
