package org.scribble.ext.ea.core.term;

import org.scribble.core.type.name.Id;
import org.scribble.ext.ea.core.term.expr.EAERec;
import org.scribble.ext.ea.core.term.expr.EAExpr;
import org.scribble.ext.ea.core.term.expr.EAEVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAVFuncType;
import org.scribble.ext.ea.core.type.value.EAVType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// !!! Currently EAName is hardcoded to Gamma.map domain (not Gamma.fmap)
public class EAFuncName extends Id implements EAExpr {

    public EAFuncName(String text) {
        super(text);
    }

    @Override
    public EAVType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAVFuncType type(Gamma gamma) {
        return gamma.fmap.get(this);
    }

    @Override
    public EAExpr subs(Map<EAEVar, EAExpr> m) {
        return this;
    }

    @Override
    public EAExpr fsubs(Map<EAFuncName, EAERec> m) {
        return m.containsKey(this) ? m.get(this) : this;
    }

    @Override
    public Set<EAEVar> getFreeVars() {
        //return Set.of();
        return new HashSet<>();
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAFuncName eaVar = (EAFuncName) o;
        return eaVar.canEquals(this) && super.equals(o);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAFuncName;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.FUNC_NAME;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
