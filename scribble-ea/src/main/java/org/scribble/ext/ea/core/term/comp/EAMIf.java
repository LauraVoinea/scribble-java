package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.*;
import org.scribble.ext.ea.core.term.expr.*;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class EAMIf implements EAComp {

    @NotNull
    public final EAExpr cond;
    @NotNull
    public final EAComp then;
    @NotNull
    public final EAComp elsee;

    public EAMIf(@NotNull EAExpr cond, @NotNull EAComp then, @NotNull EAComp elsee) {
        this.cond = cond;
        this.then = then;
        this.elsee = elsee;
    }

    @Override
    public Pair<EAVType, EALType> type(GammaState gamma, EALType pre) {
        Pair<EAVType, EALType> ttype = this.then.type(gamma, pre);
        Pair<EAVType, EALType> etype = this.elsee.type(gamma, pre);
        ////subtype(ftype.S, pre);
        //if (!ttype.equals(etype)) {
        Optional<EAVType> u = EAVType.unify(ttype.left, etype.left);
        if (!u.isPresent() || !ttype.right.equals(etype.right)) {
            throw new RuntimeException("Incompatible branches:\n"
                    + "\tfound=" + ttype + ", required=" + etype);
        }
        return new Pair<>(u.get(), ttype.right);
    }

    @Override
    public EALType infer(GammaState gamma) {
        /*EAValType ftype = this.left.type(gamma);
        if (!(ftype instanceof EAFuncType)) {
            throw new RuntimeException("Couldn't infer type: " + ftype);
        }
        return ((EAFuncType) ftype).S;  // infer yields the I/O to be done*/
        throw new RuntimeException("Not used? " + this);
    }

    @Override
    public boolean canBeta() {
        return this.cond.canEval() || this.cond instanceof EAEBoolVal;
    }

    @Override
    public EAComp beta() {
        if (!canBeta()) {
            throw new RuntimeException("Stuck: " + this);
        }
        if (this.cond.canEval()) {
            return EATermFactory.factory.iff(this.cond.eval(), this.then, this.elsee);
        } else {
            return ((EAEBoolVal) this.cond).val ? this.then : this.elsee;
        }
    }

    @Override
    public EAComp getConfigRedexCandidate() {
        return this;
    }

    @Override
    public EAComp configReduce() {
        return beta();
    }

    /* Aux */

    @Override
    public EAMIf subs(@NotNull Map<EAEVar, EAExpr> m) {
        return EATermFactory.factory.iff(
                this.cond.subs(m), this.then.subs(m), this.elsee.subs(m));
    }

    @Override
    public EAMIf fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        return EATermFactory.factory.iff(
                this.cond.fsubs(m), this.then.fsubs(m), this.elsee.fsubs(m));
    }

    @Override
    public EAComp recon(@NotNull EAComp old, EAComp neww) {
        throw new RuntimeException("Needed? " + this);
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = new HashSet<>();
        res.addAll(this.cond.getFreeVars());
        res.addAll(this.then.getFreeVars());
        res.addAll(this.elsee.getFreeVars());
        return res;
    }

    /*@Override
    public boolean isGround() {
        return this.cond.isGround() && this.then.isGround() && this.elsee.isGround();  // !!! bad naming
    }*/

    @Override
    public String toString() {
        return "if " + this.cond + " then " + this.then + " else " + this.elsee;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMIf them = (EAMIf) o;
        return them.canEquals(this)
                && this.cond.equals(them.cond)
                && this.then.equals(them.then)
                && this.elsee.equals(them.elsee);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMIf;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.IF;
        hash = 31 * hash + this.cond.hashCode();
        hash = 31 * hash + this.then.hashCode();
        hash = 31 * hash + this.elsee.hashCode();
        return hash;
    }
}
