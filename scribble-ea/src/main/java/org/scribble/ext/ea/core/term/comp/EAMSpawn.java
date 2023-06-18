package org.scribble.ext.ea.core.term.comp;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.runtime.EAAsyncSystem;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.term.EATermFactory;
import org.scribble.ext.ea.core.term.expr.EAEFuncName;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALInType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.core.type.value.EAVUnitType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.List;
import java.util.Map;
import java.util.Set;

public class EAMSpawn implements EAComp {

    @NotNull public final EAComp M;

    public EAMSpawn(EAComp M) {
        this.M = M;
    }

    @Override
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(GammaState gamma, EALType pre) {

        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type = this.M.type(gamma, EALEndType.END);
        if (type.isLeft()) {
            return Either.left(type.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> get = type.getRight();

        // TODO FIXME spawn M V
        /*if (!get.left.left.equals(EAVUnitType.UNIT)) {  // should be same type as V in spawn M V
            return Either.left(new Exception("Expected value type "
                    + EAVUnitType.UNIT + ", not: " + get.left.left));
        }*/
        if (!get.left.right.equals(EALEndType.END)) {
            return Either.left(new Exception("Expected session type "
                    + EALEndType.END + ", not: " + get.left.right));
        }
        return Either.right(Pair.of(
                get.left,
                Tree.of("[T-Spawn] " + toTypeJudgeString(
                        gamma, pre, get.left.left, get.left.right), get.right)
        ));
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
        // cf. EAMSend
        return Either.right(Pair.of(
                EATermFactory.factory.returnn(EATermFactory.factory.unit()),
                Tree.of("[..E-Ctx-Leaf-Spawn..]")  // actual [E-Spawn] in EAAsyncSystem (cf. config reduction)  // E for eval (not E-context)
        ));
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
