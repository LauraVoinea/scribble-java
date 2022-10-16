package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.util.Pair;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

public class EAPLet implements EAPExpr {

    @NotNull public final EAPVar var;
    @NotNull public final EAValType varType;  // vars x have ValTypes A -- !!! added type annot
    //@NotNull public final EAPExpr init;  // !!! value?  not expr
    @NotNull public final EAPExpr init;
    @NotNull public final EAPExpr body;

    //public EAPLet(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAPLet(@NotNull EAPVar var, @NotNull EAValType varType,
                  @NotNull EAPExpr init, @NotNull EAPExpr body) {
        this.var = var;
        this.varType = varType;
        this.init = init;
        this.body = body;
    }

    @Override
    public Pair<EAValType, EALType> type(Gamma gamma, EALType pre) {
        Pair<EAValType, EALType> p1 = this.init.type(gamma, pre);
        if (!this.varType.equals(p1.left)) {
            throw new RuntimeException("Bad type annotation: "
                    + this.varType + ", " + p1.left);
        }
        LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, p1.left);
        Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap));
        return this.body.type(gamma1, p1.right);
    }

    @Override
    public EALType infer(Gamma gamma) {
        EALType i = this.init.infer(gamma);
        LinkedHashMap<EAName, EAValType> tmp = new LinkedHashMap<>(gamma.map);
        tmp.put(this.var, this.varType);
        Gamma gamma1 = new Gamma(tmp, new LinkedHashMap<>(gamma.fmap));
        EALType b = this.body.infer(gamma1);
        return i.concat(b);
    }

    @Override
    public boolean canBeta() {
        return this.init.canBeta() || (this.init instanceof EAPReturn && this.init.isGround());  // FIXME: refact Return.canBeta
    }

    @Override
    public EAPExpr beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.init.canBeta()) {
            return EAPFactory.factory.let(this.var, this.varType, this.init.beta(), this.body);
        } else {  // this.init instanceof EAPReturn && this.init.isGround()
            return this.body.subs(Map.of(this.var, ((EAPReturn) this.init).val));
        }
    }

    /* Aux */

    @Override
    public EAPLet subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPExpr init1 = this.init.subs(m);
        Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAPExpr body1 = body.subs(m1);
        return EAPFactory.factory.let(this.var, this.varType, init1, body1);
    }

    @Override
    public EAPLet fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        EAPExpr init1 = this.init.fsubs(m);
        EAPExpr body1 = body.fsubs(m);
        return EAPFactory.factory.let(this.var, this.varType, init1, body1);
    }

    @Override
    public EAPExpr recon(@NotNull EAPExpr old, EAPExpr neww) {
        EAPExpr init1 = this.init.recon(old, neww);
        return EAPFactory.factory.let(this.var, this.varType, init1, this.body);  // !!! CHECKME: body unchanged
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = this.init.getFreeVars();
        res.addAll(this.body.getFreeVars());
        res.remove(this.var);
        return res;
    }

    @Override
    public boolean isGround() {
        return this.init.isGround();  // !!! bad naming
    }

    @Override
    public EAPExpr getFoo() {
        return this.init.getFoo();
    }

    @Override
    public String toString() {
        return "let " + this.var + ":" + this.varType
                + " <= " + this.init + " in " + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPLet them = (EAPLet) o;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.varType.equals(them.varType)
                && this.init.equals(them.init)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPLet;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.LET;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.varType.hashCode();
        hash = 31 * hash + this.init.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }
}
