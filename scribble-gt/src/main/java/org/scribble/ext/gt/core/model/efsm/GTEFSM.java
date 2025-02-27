package org.scribble.ext.gt.core.model.efsm;

import org.scribble.ext.gt.core.model.efsm.event.GTVAction;
import org.scribble.ext.gt.core.model.efsm.event.GTVEvent;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class GTEFSM {

    public final Set<GTVState> S;
    public final GTVState init;
    public final Set<GTVEvent> E;
    public final Set<GTVAction> A;
    public final Map<Pair<GTVState, GTVEvent>, Pair<GTVAction, GTVState>> delta;

    public GTEFSM(Set<GTVState> S, GTVState init, Set<GTVEvent> E, Set<GTVAction> A,
                  Map<Pair<GTVState, GTVEvent>, Pair<GTVAction, GTVState>> delta) {
        this.S = Collections.unmodifiableSet(new LinkedHashSet<>(S));
        this.init = init;
        this.E = Collections.unmodifiableSet(new LinkedHashSet<>(E));
        this.A = Collections.unmodifiableSet(new LinkedHashSet<>(A));
        this.delta = delta.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));
    }

    public String toDot() {
        StringBuilder b = new StringBuilder();
        b.append("digraph G {\ncompound = true;\n");
        for (GTVState s : this.S) {
            b.append("\"");
            b.append(s.id);
            b.append("\" [ label=\"");
            b.append(s.id);
            b.append(":\" ];\n");
        }
        for (Map.Entry<Pair<GTVState, GTVEvent>, Pair<GTVAction, GTVState>> x : this.delta.entrySet()) {
            Pair<GTVState, GTVEvent> k = x.getKey();
            Pair<GTVAction, GTVState> v = x.getValue();
            b.append("\"");
            b.append(k.left);
            b.append("\" -> \"");
            b.append(v.right);
            b.append("\" [ label=\"");
            b.append(k.right);
            b.append("/");
            b.append(v.left);
            b.append("\" ];\n");
        }
        b.append("}");
        return b.toString();
    }

    @Override
    public String toString() {
        return this.S + ", " + this.init + ", " + this.E + ", " + this.A + ", " + this.delta;
    }


    /* ... */

    @Override
    public int hashCode() {
        int hash = 15443;
        hash = 31 * hash + this.S.hashCode();
        hash = 31 * hash + this.init.hashCode();
        hash = 31 * hash + this.E.hashCode();
        hash = 31 * hash + this.A.hashCode();
        hash = 31 * hash + this.delta.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof GTEFSM cast)) {
            return false;
        }
        return this.S.equals(cast.S) && this.init.equals(cast.init)
                && this.E.equals(cast.E) && this.A.equals(cast.A)
                && this.delta.equals(cast.delta);
    }
}
