package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.ext.ea.util.EAPPair;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EAPIf implements EAPExpr {

    @NotNull
    public final EAPVal cond;
    @NotNull
    public final EAPExpr then;
    @NotNull
    public final EAPExpr elsee;

    public EAPIf(@NotNull EAPVal cond, @NotNull EAPExpr then, @NotNull EAPExpr elsee) {
        this.cond = cond;
        this.then = then;
        this.elsee = elsee;
    }

    @Override
    public EAPPair<EAValType, EALType> type(Gamma gamma, EALType pre) {
        EAPPair<EAValType, EALType> ttype = this.then.type(gamma, pre);
        EAPPair<EAValType, EALType> etype = this.elsee.type(gamma, pre);
        //subtype(ftype.S, pre);
        if (!ttype.equals(etype)) {
            throw new RuntimeException("Incompatible branches:\n"
                    + "\tfound=" + ttype + ", required=" + etype);
        }
        return ttype;
    }

    @Override
    public EALType infer(Gamma gamma) {
        /*EAValType ftype = this.left.type(gamma);
        if (!(ftype instanceof EAFuncType)) {
            throw new RuntimeException("Couldn't infer type: " + ftype);
        }
        return ((EAFuncType) ftype).S;  // infer yields the I/O to be done*/
        throw new RuntimeException("Not used? " + this);
    }

    @Override
    public boolean canBeta() {
        return this.cond.canBeta() || this.cond instanceof EAPBoolVal;
    }

    @Override
    public EAPExpr beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.cond.canBeta()) {
            return EAPFactory.factory.iff(this.cond.beta(), this.then, this.elsee);
        } else {
            return ((EAPBoolVal) this.cond).val ? this.then : this.elsee;
        }
    }

    /* Aux */

    @Override
    public EAPIf subs(@NotNull Map<EAPVar, EAPVal> m) {
        return EAPFactory.factory.iff(
                this.cond.subs(m), this.then.subs(m), this.elsee.subs(m));
    }

    @Override
    public EAPIf fsubs(@NotNull Map<EAPFuncName, EAPRec> m) {
        return EAPFactory.factory.iff(
                this.cond.fsubs(m), this.then.fsubs(m), this.elsee.fsubs(m));
    }

    @Override
    public EAPExpr recon(@NotNull EAPExpr old, EAPExpr neww) {
        throw new RuntimeException("Needed? " + this);
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        Set<EAPVar> res = new HashSet<>();
        res.addAll(this.cond.getFreeVars());
        res.addAll(this.then.getFreeVars());
        res.addAll(this.elsee.getFreeVars());
        return res;
    }

    @Override
    public boolean isGround() {
        return this.cond.isGround() && this.then.isGround() && this.elsee.isGround();  // !!! bad naming
    }

    @Override
    public EAPExpr getFoo() {
        return this;
    }

    @Override
    public EAPExpr foo() {
        return beta();
    }

    @Override
    public String toString() {
        return "if " + this.cond + " then " + this.then + " else " + this.elsee;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPIf them = (EAPIf) o;
        return them.canEquals(this)
                && this.cond.equals(them.cond)
                && this.then.equals(them.then)
                && this.elsee.equals(them.elsee);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPIf;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.IF;
        hash = 31 * hash + this.cond.hashCode();
        hash = 31 * hash + this.then.hashCode();
        hash = 31 * hash + this.elsee.hashCode();
        return hash;
    }
}
