package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAHandlersType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Set;

// !!! encodable using suspend? apart from "GC" flavour
public class EAPReturn implements EAPExpr {

    @NotNull
    public final EAPVal val;  // value, not expr

    public EAPReturn(EAPVal val) {
        this.val = val;
    }

    @Override
    public EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre) {
        EALEndType end = EATypeFactory.factory.local.end();
        /*if (!pre.equals(end)) {  // !!! return is value/term typing wrapper, not (session) control flow
            throw new RuntimeException("Expected end type: " + pre);
        }*/
        EAValType t = this.val.type(gamma);
        //return new EAPPair<>(t, end);
        return new EAPPair<>(t, pre);
    }

    @Override
    public EALEndType infer(Gamma gamma) {
        return EALEndType.END;  // !!! (potential) placeholder
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
    public EAPReturn subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.returnn(val1);
    }

    @Override
    public EAPReturn fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        EAPVal val1 = this.val.fsubs(m);
        return EAPFactory.factory.returnn(val1);
    }

    @Override
    public EAPReturn recon(@NotNull EAPExpr old, EAPExpr neww) {
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

    @Override
    public EAPExpr getFoo() {
        return this;
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
        EAPReturn eaVar = (EAPReturn) o;
        return eaVar.canEquals(this) && this.val.equals(eaVar.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPReturn;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.RETURN;
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
