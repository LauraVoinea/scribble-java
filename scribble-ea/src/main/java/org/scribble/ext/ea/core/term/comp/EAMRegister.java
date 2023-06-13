package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class EAMRegister implements EAComp {

    @NotNull public final EAExpr V;
    @NotNull public final Role role;
    @NotNull public final EAComp M;

    public EAMRegister(EAExpr V, Role role, EAComp M) {
        this.V = V;
        this.role = role;
        this.M = M;
    }

    @Override
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {
        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public EALType infer(GammaState gamma) {
        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> beta() {
        return Either.left(new Exception("Stuck: " + this));
    }

    @Override
    public EAComp getStepSubexprE() {
        return this;
    }

    @Override
    public Either<Exception, Pair<EAComp, Tree<String>>> contextStepE() {

        // !!! register reduction uses context M (not E) -- ...make getStepSubexprM ? (in active thread)

        throw new RuntimeException("TODO: " + this);
    }

    /* Aux */

    @Override
    public EAMRegister subs(Map<EAEVar, EAExpr> m) {
        return EATermFactory.factory.register(
                this.V.subs(m), this.role, this.M.subs(m));
    }

    @Override
    public EAMRegister fsubs(Map<EAEFuncName, EAERec> m) {
        return EATermFactory.factory.register(
                this.V.fsubs(m), this.role, this.M.fsubs(m));
    }

    @Override
    public EAComp recon(EAComp old, EAComp neww) {
        throw new RuntimeException("Needed? " + this);
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        Set<EAEVar> res = new HashSet<>();
        res.addAll(this.V.getFreeVars());
        res.addAll(this.M.getFreeVars());
        return res;
    }

    @Override
    public String toString() {
        return "register " + this.V + " " + this.role + " " + this.M;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMRegister them = (EAMRegister) o;
        return them.canEquals(this)
                && this.V.equals(them.V)
                && this.role.equals(them.role)
                && this.M.equals(them.M);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMRegister;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.REGISTER;
        hash = 31 * hash + this.V.hashCode();
        hash = 31 * hash + this.role.hashCode();
        hash = 31 * hash + this.M.hashCode();
        return hash;
    }
}
