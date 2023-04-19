package org.scribble.ext.ea.core.term;

import org.scribble.core.type.name.Id;
import org.scribble.ext.ea.core.term.expr.EAPRec;
import org.scribble.ext.ea.core.term.expr.EAPExpr;
import org.scribble.ext.ea.core.term.expr.EAPVar;
import org.scribble.ext.ea.core.type.Gamma;
import org.scribble.ext.ea.core.type.value.EAFuncType;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

// !!! Currently EAName is hardcoded to Gamma.map domain (not Gamma.fmap)
public class EAPFuncName extends Id implements EAPExpr {

    public EAPFuncName(String text) {
        super(text);
    }

    @Override
    public EAValType infer() {
        throw new RuntimeException("Not supported");
    }

    @Override
    public boolean canBeta() {
        return false;
    }

    @Override
    public EAPExpr beta() {
        throw new RuntimeException("Stuck: " + this);
    }

    @Override
    public EAFuncType type(Gamma gamma) {
        return gamma.fmap.get(this);
    }

    @Override
    public EAPExpr subs(Map<EAPVar, EAPExpr> m) {
        return this;
    }

    @Override
    public EAPExpr fsubs(Map<EAPFuncName, EAPRec> m) {
        return m.containsKey(this) ? m.get(this) : this;
    }

    @Override
    public Set<EAPVar> getFreeVars() {
        //return Set.of();
        return new HashSet<>();
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAPFuncName eaVar = (EAPFuncName) o;
        return eaVar.canEquals(this) && super.equals(o);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAPFuncName;
    }

    @Override
    public int hashCode() {
        int hash = EAPTerm.FUNC_NAME;
        hash = 31 * hash + super.hashCode();
        return hash;
    }
}
