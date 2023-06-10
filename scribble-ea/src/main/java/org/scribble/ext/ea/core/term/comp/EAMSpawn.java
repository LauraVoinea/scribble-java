package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Set;

public class EAMSpawn implements EAComp {

    @NotNull public final EAComp M;

    public EAMSpawn(EAComp M) {
        this.M = M;
    }

    @Override
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {

        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public EALInType infer(GammaState gamma) {
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

        // !!! spawn reduction uses context M (not E) -- ...make getStepSubexprM ?

        throw new RuntimeException("TODO: " + this);
    }

    /* Aux */

    @Override
    public EAMSpawn subs(@NotNull Map<EAEVar, EAExpr> m) {
        EAComp M = this.M.subs(m);
        return EATermFactory.factory.spawn(M);
    }

    @Override
    public EAMSpawn fsubs(@NotNull Map<EAEFuncName, EAERec> m) {
        EAComp M = this.M.fsubs(m);
        return EATermFactory.factory.spawn(M);
    }

    @Override
    public EAMSpawn recon(@NotNull EAComp old, EAComp neww) {
        EAComp M = this.M.recon(old, neww);
        return EATermFactory.factory.spawn(M);
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        return this.M.getFreeVars();
    }

    @Override
    public String toString() {
        return "spawn " + this.M;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAMSpawn them = (EAMSpawn) o;
        return them.canEquals(this) && this.M.equals(them.M);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAMSpawn;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.SPAWN;
        hash = 31 * hash + this.M.hashCode();
        return hash;
    }
}
