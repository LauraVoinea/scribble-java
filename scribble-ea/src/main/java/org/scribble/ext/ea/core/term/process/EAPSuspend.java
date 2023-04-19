package org.scribble.ext.ea.core.term.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAPHandlers;
import org.scribble.ext.ea.core.term.expr.EAPRec;
import org.scribble.ext.ea.core.term.expr.EAPExpr;
import org.scribble.ext.ea.core.term.expr.EAPVar;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAHandlersType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.Map;
import java.util.Set;

public class EAPSuspend implements EAComp {

    @NotNull
    public final EAPExpr val;  // value, not expr -- a Handler(S?) type

    @NotNull
    public final EAPExpr sval;

    public EAPSuspend(@NotNull EAPExpr val, @NotNull EAPExpr sval) {
        this.val = val;
        this.sval = sval;
    }

    @Override
    public EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre) {
        //if (!(pre instanceof EALInType)) {
        if (!EAPApp.isInType(pre)) {  // Could be a rec type
            throw new RuntimeException("Expected in type, not: " + pre);
        }
        EAValType t = this.val.type(gamma);
        if (!(t instanceof EAHandlersType)) {
            throw new RuntimeException("Expected handlers type, not: " + this);
        }
        EAHandlersType cast = (EAHandlersType) t;
        if (!cast.T.equals(gamma.svarType)) {
            throw new RuntimeException("Incompatible state type: found=" + cast.T + ", gamma=" + gamma.svarType);
        }
        /*if (!(cast.S.equals(pre))) {
            throw new RuntimeException("Incompatible in type: " + pre + ", " + cast.S);
        }*/
        EAPApp.subtype(cast.S, pre);

        EAValType t_s = this.sval.type(gamma);
        if (!t_s.equals(gamma.svarType)) {
            throw new RuntimeException("Expected state type " + gamma.svarType + ", not: " + t_s);
        }

        EATypeFactory tf = EATypeFactory.factory;
        return new EAPPair<>(tf.val.unit(), tf.local.end());  // !!! unit vs A. !!! end vs. S'
    }

    @Override
    public EALInType infer(Gamma gamma) {
        EAHandlersType ht;
        if (this.val instanceof EAPHandlers) {
            ht = (EAHandlersType) this.val.type(gamma);
        } else if (this.val instanceof EAPExpr) {
            ht = (EAHandlersType) gamma.map.get(this.val);
        } else {
            throw new RuntimeException("Shouldn't get here: " + gamma);
        }
        return (EALInType) ht.S;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAComp beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /* Aux */

    @Override
    public EAPSuspend subs(@NotNull Map<EAPVar, EAPExpr> m) {
        EAPExpr val1 = this.val.subs(m);
        EAPExpr sval1 = this.sval.subs(m);
        return EAPFactory.factory.suspend(val1, sval1);
    }

    @Override
    public EAPSuspend fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        EAPExpr val1 = this.val.fsubs(m);
        EAPExpr sval1 = this.sval.fsubs(m);
        return EAPFactory.factory.suspend(val1, sval1);
    }

    @Override
    public EAPSuspend recon(@NotNull EAComp old, EAComp neww) {
        return this;
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        return this.val.getFreeVars();
    }

    @Override
    public boolean isGround() {
        return this.val.isGround();
    }

    /*@Override
    public boolean canFoo() {
        return true;
    }*/

    @Override
    public EAComp getConfigRedexCandidate() {
        return this;
    }

    @Override
    public EAComp configStep() {
        throw new RuntimeException("Shouldn't get in here.");
    }

    @Override
    public String toString() {
        return "suspend " + this.val + ", " + this.sval;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSuspend eaVar = (EAPSuspend) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val)
                && this.sval.equals(eaVar.sval);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSuspend;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.SUSPEND;
        hash = 31 * hash + this.val.hashCode();
        hash = 31 * hash + this.sval.hashCode();
        return hash;
    }
}
