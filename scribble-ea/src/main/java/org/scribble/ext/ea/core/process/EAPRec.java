package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.config.EAPRuntimeFactory;
import org.scribble.ext.ea.core.type.EATypeFactory;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAFuncType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.core.type.value.EAValTypeFactory;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.util.Pair;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAPRec implements EAPVal {

    @NotNull
    public final EAPFuncName f;
    @NotNull
    public final EAPVar var;  // hardcoded single param
    @NotNull
    public final EAValType varType;  // A -- hardcode single param
    @NotNull
    public final EAPExpr body;

    @NotNull
    public final EALType S;
    @NotNull
    public final EALType T;
    @NotNull
    public final EAValType B;

    //public EAPLet(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAPRec(@NotNull EAPFuncName f, @NotNull EAPVar var,
                  @NotNull EAValType varType, @NotNull EAPExpr body,
                  @NotNull EALType S, @NotNull EALType T, @NotNull EAValType B) {
        this.f = f;
        this.var = var;
        this.varType = varType;
        this.body = body;

        this.S = S;
        this.T = T;
        this.B = B;
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPVal beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAValType type(Gamma gamma) {
        LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, this.varType);
        LinkedHashMap<EAPFuncName, EAFuncType> ftmp = new LinkedHashMap<>(gamma.fmap);
        EAFuncType ftype = EATypeFactory.factory.val.func(this.varType, this.S, this.T, this.B);
        ftmp.put(this.f, ftype);
        Gamma gamma1 = new Gamma(tmp, ftmp);
        EAPPair<EAValType, EALType> res = this.body.type(gamma1, this.S);
        EAPPair<EAValType, EALType> target = new EAPPair<>(this.B, this.S);
        if (!res.equals(target)) {
            throw new RuntimeException("Typing error:\n\t" + res + "\n\t" + target);
        }
        return ftype;
    }


    /* Aux */

    @Override
    public EAPRec subs(@NotNull Map<EAPVar, EAPVal> m) {
        Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAPExpr body1 = body.subs(m1);
        return EAPFactory.factory.rec(this.f, this.var, this.varType, body1,
                this.S, this.T, this.B);
    }

    @Override
    public EAPVal fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        Map<EAPFuncName, EAPRec> m1 = new HashMap<>(m);
        m1.remove(this.f);
        EAPExpr body1 = body.fsubs(m1);
        return EAPFactory.factory.rec(this.f, this.var, this.varType, body1,
                this.S, this.T, this.B);
    }

    // CHECKME: how about free f names?
    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = this.body.getFreeVars();
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
        EAPRec them = (EAPRec) o;
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
        return o instanceof EAPRec;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.REC;
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
