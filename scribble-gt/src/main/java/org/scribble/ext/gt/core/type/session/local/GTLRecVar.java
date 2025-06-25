package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVRecVar;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.GTVRecv;
import org.scribble.util.Pair;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class GTLRecVar implements GTLType {

    public final RecVar var;

    protected GTLRecVar(RecVar var) {
        this.var = var;
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return this.equals(t) ? Optional.of(this) : Optional.empty();
    }

    @Override
    public GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                            int c, GTVState s, GTVState end) {
        GTVRecVar s1 = new GTVRecVar(c, this.var);
        return new GTEFSM(Set.of(s1), s1, Set.of(), Set.of(), Map.of());
    }


    /* ... */

    @Override
    public GTLType subs(RecVar rv, GTLType t) {
        return this.var.equals(rv) ? t : this;
    }

    @Override
    public String toString() {
        return this.var.toString();
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.RECVAR_HASH;
        hash = 31 * hash + this.var.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLRecVar)) { return false; }
        GTLRecVar them = (GTLRecVar) obj;
        return them.canEquals(this)
                && this.var.equals(them.var);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLRecVar;
    }


}
