package org.scribble.ext.ea.core.term.expr;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.term.EAName;
import org.scribble.ext.ea.core.term.EATerm;
import org.scribble.ext.ea.core.type.GammaState;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.ext.ea.util.EAUtil;
import org.scribble.ext.ea.util.Either;
import org.scribble.ext.ea.util.Tree;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Objects;
import java.util.Set;

// HERE HERE is AP name c a val V?(cf.value c)

// c, c', ...
public class EAEAPName implements EAExpr, EAName {

    public final String id;

    public EAEAPName(String id) {
        this.id = id;
    }

    @Override
    public EAVType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public Either<Exception, Pair<EAVType, Tree<String>>> type(GammaState gamma) {

        // APType size >1 -- check here or "WF"? -- ...currently parsing

        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public Either<Exception, Pair<EAExpr, Tree<String>>> eval() {
        return Either.left(newStuck());
    }

    /* Aux */

    @Override
    public EAExpr subs(@NotNull Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAEFuncName, EAERec> m) { return this; }

    @Override
    public Set<EAEVar> getFreeVars() {
        return EAUtil.setOf();
    }

    @Override
    public boolean isValue() {
        return true;
    }

    @Override
    public String toString() {
        return this.id;
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAEAPName eaVar = (EAEAPName) o;
        return eaVar.canEquals(this) && Objects.equals(this.id, eaVar.id);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAEAPName;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.AP_NAME;
        hash = 31 * hash + this.id.hashCode();
        return hash;
    }
}
