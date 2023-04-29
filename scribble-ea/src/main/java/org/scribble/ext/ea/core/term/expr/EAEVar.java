package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

// x, y, ...
public class EAEVar implements EAExpr, EAName {

    public final String id;

    public EAEVar(String id) {
        this.id = id;
    }

    @Override
    public EAVType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    //public EAVType type(GammaState gamma) {
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        if (gamma.gamma.map.containsKey(this)) {
            //return gamma.gamma.map.get(this);
            return Either.right(new Pair<>(gamma.gamma.map.get(this), new Tree<>("..name..")));
        }
        //throw new RuntimeException("Unknown var: " + this + ", " + gamma);
        return Either.left(new Exception("Unknown var: " + this + ", " + gamma));
    }

    /* Aux */

    /*@Override
    public boolean isGround() {
        return false;
    }*/

    @Override
    public EAExpr subs(@NotNull Map<EAEVar, EAExpr> m) {
        return m.containsKey(this) ? m.get(this) : this;
    }

    @Override
    public EAExpr fsubs(Map<EAEFuncName, EAERec> m) { return this; }

    @Override
    public Set<EAEVar> getFreeVars() {
        //return Set.of(this);
        return new HashSet<>(Set.of(this));
    }

    @Override
    public boolean isValue() {
        return false;
    }

    /*@Override
    public boolean isGround() {
        return true;
    }*/

    @Override
    public String toString() {
        return this.id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAEVar eaVar = (EAEVar) o;
        return eaVar.canEquals(this) && Objects.equals(this.id, eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEVar;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.VAR;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
