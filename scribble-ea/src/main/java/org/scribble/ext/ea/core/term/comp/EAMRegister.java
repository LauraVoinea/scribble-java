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
import org.scribble.ext.ea.core.type.session.local.EALEndType;
import org.scribble.ext.ea.core.type.session.local.EALType;
import org.scribble.ext.ea.core.type.value.EAVAPType;
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
    public Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type(
            GammaState gamma, EALType pre) {

        // cf. EAMSpawn

        System.err.println("[Warning] TODO check register V AP type: " + this.V);  // Need to add sys.access info to gamma

        Either<Exception, Pair<EAVType, Tree<String>>> type_V = this.V.type(gamma);
        if (type_V.isLeft()) { return Either.left(type_V.getLeft()); }
        Pair<EAVType, Tree<String>> t_V = type_V.getRight();  // TODO deriv
        if (!(t_V.left instanceof EAVAPType)) {
            return Either.left(new Exception("Expected AP type, not: " + t_V.left));
        }
        EAVAPType t_ap = (EAVAPType) t_V.left;
        if (!t_ap.roles.containsKey(this.role)) {
            return Either.left(new Exception("Unknown role " + this.role + " in :" + t_ap));
        }

        Either<Exception, Pair<Pair<EAVType, EALType>, Tree<String>>> type =
                this.M.type(gamma, t_ap.roles.get(this.role));
        if (type.isLeft()) {
            return Either.left(type.getLeft());
        }
        Pair<Pair<EAVType, EALType>, Tree<String>> get = type.getRight();

        System.err.println("[Warning] TODO check register M value type: " + get.left.left);  // cf. state type

        if (!get.left.right.equals(EALEndType.END)) {
            return Either.left(new Exception("Expected session type "
                    + EALEndType.END + ", not: " + get.left.right));
        }

        return Either.right(Pair.of(
                get.left,
                Tree.of("[T-Register] " + toTypeJudgeString(
                        gamma, pre, get.left.left, get.left.right), get.right)
        ));

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
        // cf. EAMSend, EAMSpawn
        return Either.right(Pair.of(
                EATermFactory.factory.returnn(EATermFactory.factory.unit()),
                Tree.of("[..E-Ctx-Leaf-Register..]")  // actual [E-Register] in EAAsyncSystem (cf. config reduction)  // E- for eval (not E-context)
        ));
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
        if (this == o) { return true; }
        if (o == null || getClass() != o.getClass()) { return false; }
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
