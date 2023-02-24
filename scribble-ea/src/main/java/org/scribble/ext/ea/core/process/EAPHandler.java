package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.session.local.EALTypeFactory;
import org.scribble.ext.ea.core.type.value.EAUnitType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.core.type.value.EAValTypeFactory;
import org.scribble.ext.ea.util.ConsoleColors;
import org.scribble.ext.ea.util.EAPPair;
import org.scribble.ext.ea.util.EATriple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class EAPHandler {

    @NotNull public final Op op;
    @NotNull public final EAPVar var;
    @NotNull public final EAValType varType;  // !!! added type annots
    @NotNull public final EAPExpr expr;
    @NotNull public final EALType pre;  // For the handler expr (i.e., excl. the handler input itself)

    protected EAPHandler(@NotNull Op op, @NotNull EAPVar var, @NotNull EAValType varType,
                         @NotNull EAPExpr expr, @NotNull EALType pre) {
        this.op = op;
        this.var = var;
        this.varType = varType;
        this.expr = expr;
        this.pre = pre;
    }

    public void type(Gamma gamma) {
       EATriple<EAPVar, EAValType, EAPExpr> v;
       LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
       tmp.put(this.var, this.varType);
       Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap));

       //EALType inferred = this.expr.infer(gamma1);  // !!! FIXME re. [EV-Handler], S_i
       EALType inferred = this.pre;

       Pair<EAValType, EALType> res = this.expr.type(gamma1, inferred);
       if (!(res.left.equals(EAUnitType.UNIT)) || !(res.right.equals(EALEndType.END))) {
           throw new RuntimeException("Type error: " + gamma1 + " |- "
                   + inferred + " |>" + this.expr + ":" + res.left + " <|" + res.right);
       }
    }

    /* Aux */

    public EAPHandler subs(@NotNull Map<EAPVar, EAPVal> m) {
        Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAPExpr subs = this.expr.subs(m1);
        return EAPFactory.factory.handler(this.op, this.var, this.varType, subs, this.pre);
    }

    public EAPHandler fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        Map<EAPFuncName, EAPRec> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAPExpr subs = this.expr.fsubs(m1);
        return EAPFactory.factory.handler(this.op, this.var, this.varType, subs, this.pre);
    }

    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> fvs = this.expr.getFreeVars();
        fvs.remove(this.var);
        return fvs;
    }

    @Override
    public String toString() {
        return this.op + "(" + this.var
                + ConsoleColors.toAnnotString( ": " + this.varType.toString())
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
                && this.pre.equals(them.pre);
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.HANDLER;
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.varType.hashCode();
        hash = 31 * hash + this.expr.hashCode();
        hash = 31 * hash + this.pre.hashCode();
        return hash;
    }
}
