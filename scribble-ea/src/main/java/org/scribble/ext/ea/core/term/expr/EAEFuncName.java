package org.scribble.ext.ea.core.term.expr;

import org.scribble.core.type.name.Id;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// !!! Currently EAName is hardcoded to Gamma.map domain (not Gamma.fmap)
public class EAEFuncName extends Id implements EAExpr {

    public EAEFuncName(String text) {
        super(text);
    }

    /* ... */

    @Override
    public EAVType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {
        return Either.right(new Pair<>(gamma.gamma.fmap.get(this), new Tree<>("[..funcname..]")));
    }

    @Override
    public Either<Exception, Pair<EAExpr, Tree<String>>> eval() {
        return Either.left(new Exception("Stuck: " + this));
    }

    /* ... */

    @Override
    public EAExpr subs(Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAEFuncName, EAERec> m) {
        return m.containsKey(this) ? m.get(this) : this;
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        //return Set.of();
        return new HashSet<>();
    }

    @Override
    public boolean isValue() {
        return true;  // though only in body of rec f in an app, where beta_M will subs it for rec f ...
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAEFuncName eaVar = (EAEFuncName) o;
        return eaVar.canEquals(this) && super.equals(o);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEFuncName;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.FUNC_NAME;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
