package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.process.EAComp;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAPHandler {

    @NotNull
    public final Op op;
    @NotNull
    public final EAPVar var;
    @NotNull
    public final EAValType varType;  // !!! added type annots
    @NotNull
    public final EAComp expr;
    @NotNull
    public final EALType pre;  // For the handler expr (i.e., doesn't include the handler input itself)

    @NotNull
    public final EAPVar svar;
    @NotNull
    public final EAValType svarType;

    public EAPHandler(@NotNull Op op, @NotNull EAPVar var, @NotNull EAValType varType,
                      @NotNull EAComp expr, @NotNull EALType pre, @NotNull EAPVar svar, @NotNull EAValType svarType) {
        this.op = op;
        this.var = var;
        this.varType = varType;
        this.expr = expr;
        this.pre = pre;
        this.svar = svar;
        this.svarType = svarType;
    }

    public void type(Gamma gamma) {

        if (!this.svarType.equals(gamma.svarType)) {
            throw new RuntimeException("Expected state type " + gamma.svarType + ", not: " + this.svarType);
        }

        //EATriple<EAPVar, EAValType, EAPExpr> v;
        LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, this.varType);

        tmp.put(this.svar, this.svarType);  // !!! map contains smap

        Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap), this.svar, this.svarType);

        //EALType inferred = this.expr.infer(gamma1);  // !!! FIXME re. [EV-Handler], S_i
        EALType inferred = this.pre;

        Pair<EAValType, EALType> res = this.expr.type(gamma1, inferred);
        if (!(res.left.equals(EAUnitType.UNIT)) || !(res.right.equals(EALEndType.END))) {
            throw new RuntimeException("Type error: " + gamma1 + " | "
                    + inferred + " |>" + this.expr + ":" + res.left + " <|" + res.right);
        }
    }

    /* Aux */

    public EAPHandler subs(@NotNull Map<EAPVar, EAPExpr> m) {
        Map<EAPVar, EAPExpr> m1 = new HashMap<>(m);
        m1.remove(this.var);
        m1.remove(this.svar);
        EAComp subs = this.expr.subs(m1);
        return EAPFactory.factory.handler(
                this.op, this.var, this.varType, subs, this.pre, this.svar, this.svarType);
    }

    public EAPHandler fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        Map<EAPFuncName, EAPRec> m1 = new HashMap<>(m);
        m1.remove(this.var);
        m1.remove(this.svar);
        EAComp subs = this.expr.fsubs(m1);
        return EAPFactory.factory.handler(
                this.op, this.var, this.varType, subs, this.pre, this.svar, this.svarType);
    }

    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> fvs = this.expr.getFreeVars();
        fvs.remove(this.var);
        fvs.remove(this.svar);
        return fvs;
    }

    @Override
    public String toString() {
        return this.svar + ConsoleColors.toAnnotString(": " + this.svarType)
                + ", " + this.op + "(" + this.var
                + ConsoleColors.toAnnotString(": " + this.varType.toString())
                + ") "
                + ConsoleColors.toAnnotString(": " + this.pre)
                + " |-> " + this.expr;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPHandler them = (EAPHandler) o;
        return this.op.equals(them.op)
                && this.var.equals(them.var)
                && this.varType.equals(them.varType)
                && this.expr.equals(them.expr)
                && this.pre.equals(them.pre)
                && this.svar.equals(them.svar)
                && this.svarType.equals(them.svarType);
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.HANDLER;
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.varType.hashCode();
        hash = 31 * hash + this.expr.hashCode();
        hash = 31 * hash + this.pre.hashCode();
        hash = 31 * hash + this.svar.hashCode();
        hash = 31 * hash + this.svarType.hashCode();
        return hash;
    }
}
