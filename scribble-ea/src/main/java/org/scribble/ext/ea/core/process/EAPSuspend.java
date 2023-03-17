package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.visit.local.LSubprotoVisitorNoThrow;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.*;
import org.scribble.ext.ea.core.type.value.EAHandlersType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAPSuspend implements EAPExpr {

    @NotNull
    public final EAPVal val;  // value, not expr -- a Handler(S?) type

    public EAPSuspend(EAPVal val) {
        this.val = val;
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
        /*if (!(cast.S.equals(pre))) {
            throw new RuntimeException("Incompatible in type: " + pre + ", " + cast.S);
        }*/
        EAPApp.subtype(cast.S, pre);
        EATypeFactory tf = EATypeFactory.factory;
        return new EAPPair<>(tf.val.unit(), tf.local.end());  // !!! unit vs A. !!! end vs. S'
    }

    @Override
    public EALInType infer(Gamma gamma) {
        EAHandlersType ht;
        if (this.val instanceof EAPHandlers) {
            ht = (EAHandlersType) this.val.type(gamma);
        } else if (this.val instanceof EAPVal) {

            System.out.println("111: " + this.val + " ,, " + this.val.getClass());

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
    public EAPExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    /* Aux */

    @Override
    public EAPSuspend subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.suspend(val1);
    }

    @Override
    public EAPSuspend fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        EAPVal val1 = this.val.fsubs(m);
        return EAPFactory.factory.suspend(val1);
    }

    @Override
    public EAPSuspend recon(@NotNull EAPExpr old, EAPExpr neww) {
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
    public EAPExpr getFoo() {
        return this;
    }

    @Override
    public EAPExpr foo() {
        throw new RuntimeException("Shouldn't get in here.");
    }

    @Override
    public String toString() {
        return "suspend " + this.val;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSuspend eaVar = (EAPSuspend) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSuspend;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.SUSPEND;
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
