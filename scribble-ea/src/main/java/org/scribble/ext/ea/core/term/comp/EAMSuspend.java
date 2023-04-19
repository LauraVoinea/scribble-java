package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.EAEHandlers;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVHandlersType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Set;

public class EAMSuspend implements EAComp {

    @NotNull
    public final EAExpr val;  // value, not expr -- a Handler(S?) type

    @NotNull
    public final EAExpr sval;

    public EAMSuspend(@NotNull EAExpr val, @NotNull EAExpr sval) {
        this.val = val;
        this.sval = sval;
    }

    @Override
    public Pair<EAVType, EALType> type(Gamma gamma, EALType pre) {
        //if (!(pre instanceof EALInType)) {
        if (!EAMApp.isInType(pre)) {  // Could be a rec type
            throw new RuntimeException("Expected in type, not: " + pre);
        }
        EAVType t = this.val.type(gamma);
        if (!(t instanceof EAVHandlersType)) {
            throw new RuntimeException("Expected handlers type, not: " + this);
        }
        EAVHandlersType cast = (EAVHandlersType) t;
        if (!cast.T.equals(gamma.svarType)) {
            throw new RuntimeException("Incompatible state type: found=" + cast.T + ", gamma=" + gamma.svarType);
        }
        /*if (!(cast.S.equals(pre))) {
            throw new RuntimeException("Incompatible in type: " + pre + ", " + cast.S);
        }*/
        EAMApp.subtype(cast.S, pre);

        EAVType t_s = this.sval.type(gamma);
        if (!t_s.equals(gamma.svarType)) {
            throw new RuntimeException("Expected state type " + gamma.svarType + ", not: " + t_s);
        }

        EATypeFactory tf = EATypeFactory.factory;
        return new Pair<>(tf.val.unit(), tf.local.end());  // !!! unit vs A. !!! end vs. S'
    }

    @Override
    public EALInType infer(Gamma gamma) {
        EAVHandlersType ht;
        if (this.val instanceof EAEHandlers) {
            ht = (EAVHandlersType) this.val.type(gamma);
        } else if (this.val instanceof EAExpr) {
            ht = (EAVHandlersType) gamma.map.get(this.val);
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
    public EAMSuspend subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAExpr val1 = this.val.subs(m);
        EAExpr sval1 = this.sval.subs(m);
        return EATermFactory.factory.suspend(val1, sval1);
    }

    @Override
    public EAMSuspend fsubs(@NotNull Map<EAFuncName, EAERec> m) {
        EAExpr val1 = this.val.fsubs(m);
        EAExpr sval1 = this.sval.fsubs(m);
        return EATermFactory.factory.suspend(val1, sval1);
    }

    @Override
    public EAMSuspend recon(@NotNull EAComp old, EAComp neww) {
        return this;
    }

    @Override
    public Set<EAEVar> getFreeVars() {
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
        EAMSuspend eaVar = (EAMSuspend) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val)
                && this.sval.equals(eaVar.sval);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMSuspend;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.SUSPEND;
        hash = 31 * hash + this.val.hashCode();
        hash = 31 * hash + this.sval.hashCode();
        return hash;
    }
}
