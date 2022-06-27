package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.Map;
import java.util.Set;

public class EAPSend implements EAPExpr {

    @NotNull public final Role dst;
    @NotNull public final Op op;
    @NotNull public final EAPVal val;  // value, not expr

    public EAPSend(@NotNull Role dst, @NotNull Op op, @NotNull EAPVal val) {
        this.dst = dst;
        this.op = op;
        this.val = val;
    }

    @Override
    public boolean canBeta() {
        return true;
    }

    @Override
    public EAPExpr beta() {
        //throw new RuntimeException("Stuck: " + this);
        return EAPFactory.factory.returnn(EAPFactory.factory.unit());
    }

    /* Aux */

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
    public EAPSend subs(@NotNull Map<EAPVar, EAPVal> m) {
        EAPVal val1 = this.val.subs(m);
        return EAPFactory.factory.send(this.dst, this.op, val1);
    }

    @Override
    public EAPExpr recon(@NotNull EAPExpr old, EAPExpr neww) {
        return this.equals(old) ? neww : this;
    }

    @Override
    public String toString() {
        return "send " + this.dst + "!" + this.op + "(" + this.val + ")";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPSend them = (EAPSend) o;
        return them.canEquals(this)
                && this.dst.equals(them.dst)
                && this.op.equals(them.op)
                && this.val.equals(them.val);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPSend;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.SEND;
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.op.hashCode();
        hash = 31 * hash + this.val.hashCode();
        return hash;
    }
}
