package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

public class EAPLet implements EAPExpr {

    @NotNull public final EAPVar var;
    //@NotNull public final EAPExpr init;  // !!! value?  not expr
    @NotNull public final EAPVal init;
    @NotNull public final EAPExpr body;

    //public EAPLet(@NotNull EAPVar var, @NotNull EAPExpr init, @NotNull EAPExpr body) {
    public EAPLet(@NotNull EAPVar var, @NotNull EAPVal init, @NotNull EAPExpr body) {
        this.var = var;
        this.init = init;
        this.body = body;
    }

    @Override
    public boolean canBeta() {
        return this.init.isGround();
    }

    @Override
    public EAPExpr beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        return this.body.subs(Map.of(this.var, this.init));
    }

    /* Aux */

    @Override
    public EAPLet subs(@NotNull Map<EAPVar, EAPVal> m) {
        //EAPExpr init1 = this.init.subs(m);
        EAPVal init1 = this.init.subs(m);
        Map<EAPVar, EAPVal> m1 = new HashMap<>(m);
        m1.remove(this.var);
        EAPExpr body1 = body.subs(m1);
        return EAPFactory.factory.let(this.var, init1, body1);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = this.init.getFreeVars();
        res.addAll(this.body.getFreeVars());
        res.remove(this.var);
        return res;
    }

    @Override
    public String toString() {
        return "let " + this.var + " <= " + this.init + " in " + this.body;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPLet eaVar = (EAPLet) o;
        return eaVar.canEquals(this)
                && this.var.equals(eaVar.var)
                && this.init.equals(eaVar.init)
                && this.body.equals(eaVar.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPLet;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.LET;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.init.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }
}
