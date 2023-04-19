package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.process.EAComp;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.EAPPair;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAERec implements EAExpr {

    @NotNull
    public final EAFuncName f;
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
    public EAERec(@NotNull EAFuncName f, @NotNull EAEVar var,
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
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAVType type(Gamma gamma) {
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, this.varType);
        LinkedHashMap<EAFuncName, EAVFuncType> ftmp = new LinkedHashMap<>(gamma.fmap);
        EAVFuncType ftype = EATypeFactory.factory.val.func(this.varType, this.S, this.T, this.B);
        ftmp.put(this.f, ftype);
        Gamma gamma1 = new Gamma(tmp, ftmp, gamma.svar, gamma.svarType);
        EAPPair<EAVType, EALType> res = this.body.type(gamma1, this.S);
        EAPPair<EAVType, EALType> target = new EAPPair<>(this.B, this.S);
        if (!res.equals(target)) {
            throw new RuntimeException("Typing error:\n\t" + res + "\n\t" + target);
        }
        return ftype;
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
    public EAExpr fsubs(@NotNull Map<EAFuncName, EAERec> m) {
        Map<EAFuncName, EAERec> m1 = new HashMap<>(m);
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
    public boolean isGround() {
        return this.body.isGround();
    }

    @Override
    public String toString() {
        return "rec " + this.f + " "
                + ConsoleColors.toAnnotString("{ " + this.S + "}")
                + " (" + this.var + " "
                + ConsoleColors.toAnnotString(":" + this.varType)
                + ") :"
                + ConsoleColors.toAnnotString(this.B.toString() + " {" + this.T + "}")
                + " . " + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
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
