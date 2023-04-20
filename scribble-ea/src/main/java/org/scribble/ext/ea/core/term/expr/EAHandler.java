package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.comp.EAComp;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.*;

// TODO factor out with EAELam
public class EAHandler {

    @NotNull
    public final Op op;
    @NotNull
    public final EAEVar var;
    @NotNull
    public final EAVType varType;  // !!! added type annots
    @NotNull
    public final EAComp expr;
    @NotNull
    public final EALType pre;  // For the handler expr (i.e., doesn't include the handler input itself)

    @NotNull
    public final EAEVar svar;
    @NotNull
    public final EAVType svarType;

    public EAHandler(@NotNull Op op, @NotNull EAEVar var, @NotNull EAVType varType,
                     @NotNull EAComp expr, @NotNull EALType pre, @NotNull EAEVar svar, @NotNull EAVType svarType) {
        this.op = op;
        this.var = var;
        this.varType = varType;
        this.expr = expr;
        this.pre = pre;
        this.svar = svar;
        this.svarType = svarType;
    }

    public void type(GammaState gamma) {

        if (!this.svarType.equals(gamma.svarType)) {
            throw new RuntimeException("Expected state type " + gamma.svarType + ", not: " + this.svarType);
        }

        //EATriple<EAPVar, EAValType, EAPExpr> v;
        LinkedHashMap<EAName, EAVType> tmp = new LinkedHashMap<>(gamma.gamma.map);
        tmp.put(this.var, this.varType);

        tmp.put(this.svar, this.svarType);  // !!! map contains smap

        GammaState gamma1 = new GammaState(tmp, new LinkedHashMap<>(gamma.gamma.fmap), this.svarType);

        //EALType inferred = this.expr.infer(gamma1);  // !!! FIXME re. [EV-Handler], S_i
        EALType inferred = this.pre;

        Pair<EAVType, EALType> res = this.expr.type(gamma1, inferred);
        //if (!u.isPresent() || !u.get().equals(EAVUnitType.UNIT) || !res.right.equals(EALEndType.END)) {
        Optional<EAVType> u = EAVType.unify(res.left, this.svarType);
        if (!u.isPresent() || !u.get().equals(this.svarType) || !res.right.equals(EALEndType.END)) {
            throw new RuntimeException("Type error: " + gamma1 + " | "
                    + inferred + " |>" + this.expr + ":" + res.left + " <|" + res.right);
        }
    }

    /* Aux */

    public EAHandler subs(@NotNull Map<EAEVar, EAExpr> m) {
        Map<EAEVar, EAExpr> m1 = new HashMap<>(m);
        m1.remove(this.var);
        m1.remove(this.svar);
        EAComp subs = this.expr.subs(m1);
        return EATermFactory.factory.handler(
                this.op, this.var, this.varType, subs, this.pre, this.svar, this.svarType);
    }

    public EAHandler fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        Map<EAEFuncName, EAERec> m1 = new HashMap<>(m);
        m1.remove(this.var);
        m1.remove(this.svar);
        EAComp subs = this.expr.fsubs(m1);
        return EATermFactory.factory.handler(
                this.op, this.var, this.varType, subs, this.pre, this.svar, this.svarType);
    }

    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> fvs = this.expr.getFreeVars();
        fvs.remove(this.var);
        fvs.remove(this.svar);
        return fvs;
    }

    public boolean isValue() {
        return getFreeVars().isEmpty();  // i.e., isGround
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
        EAHandler them = (EAHandler) o;
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
        int hash = EATerm.HANDLER;
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
