package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.GTVRecv;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.util.Pair;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class GTLRecursion implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final RecVar var;
    public final GTLType body;

    protected GTLRecursion(RecVar var, GTLType body) {
        this.var = var;
        this.body = body;
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLRecursion)) {
            return Optional.empty();
        }
        GTLRecursion cast = (GTLRecursion) t;
        if (this.var.equals(cast.var)) {
            Optional<? extends GTLType> merge = this.body.merge(cast.body);
            return merge.map(x -> this.fact.recursion(this.var, x));
        } else {
            return Optional.empty();
        }
    }

    @Override
    public GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                            int c, GTVState s, GTVState end) {
        Set<RecVar> recvars = new LinkedHashSet<>(s.recvars);
        recvars.add(this.var);
        GTVState s1 = new GTVState(s.isEntry, c, recvars);
        return this.body.construct(r, com, recvStars, c, s1, end);

        /*GTEFSM m_body = this.body.construct(r, com, recvStars, end);
        Set<RecVar> recvars = new LinkedHashSet<>();
        recvars.add(this.var);
        recvars.addAll(m_body.init.recvars);
        GTVState init = new GTVState(recvars);

        Set<GTVState> tmp = new LinkedHashSet<>(m_body.S);
        tmp.remove(m_body.init);
        Set<GTVState> S = new LinkedHashSet<>();
        S.add(init);
        S.addAll(tmp);

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = new LinkedHashMap<>();
        for (Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> x : m_body.delta.entrySet()) {
            Pair<GTVState, GTVEvent> k = x.getKey();
            Set<Pair<GTVAction, GTVState>> v = x.getValue();
            if (k.left.equals(m_body.init)) {
                delta.put(new Pair<>(init, k.right), v);
            } else {
                delta.put(k, v);
            }
        }
        return new GTEFSM(S, init, m_body.E, m_body.A, delta);*/
    }


    /* ... */

    @Override
    public GTLType subs(RecVar rv, GTLType t) {
        if (rv.equals(this.var)) {
            return this;
        }
        return new GTLRecursion(this.var, this.body.subs(rv, t));
    }

    @Override
    public GTLType unfoldAllImmediateRecs() {
        return this.body.subs(this.var, this).unfoldAllImmediateRecs();
    }

    @Override
    public String toString() {
        return ConsoleColors.toRecString("mu " + this.var + "." + this.body);
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.REC_HASH;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLRecursion)) { return false; }
        GTLRecursion them = (GTLRecursion) obj;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLRecursion;
    }


    /* Aux */

    @Override
    public Map<Integer, Integer> getActive(Theta theta) {
        return GTUtil.mapOf();
    }
}
