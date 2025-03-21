package org.scribble.ext.gt.core.model.efsm;

import org.scribble.core.type.name.RecVar;
import org.scribble.ext.gt.core.model.efsm.event.GTVAction;
import org.scribble.ext.gt.core.model.efsm.event.GTVEvent;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;


// HERE HERE
// - use existing com to test paper examples => API gen
// - generalise com to per MC -- in getCommittingTop record recursively collected committing inside each MC

public class GTEFSM {

    public final Set<GTVState> S;
    public final GTVState init;
    public final Set<GTVEvent> E;
    public final Set<GTVAction> A;
    public final Map<
            Pair<GTVState, GTVEvent>,
            Set<Pair<GTVAction, GTVState>>> delta;

    public GTEFSM(Set<GTVState> S, GTVState init, Set<GTVEvent> E, Set<GTVAction> A,
                  Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta) {
        GTVState.resetIdCounter();
        this.S = Collections.unmodifiableSet(new LinkedHashSet<>(S));
        this.init = init;
        this.E = Collections.unmodifiableSet(new LinkedHashSet<>(E));
        this.A = Collections.unmodifiableSet(new LinkedHashSet<>(A));
        this.delta = delta.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> Collections.unmodifiableSet(new LinkedHashSet<>(x.getValue())),
                (x, y) -> null,
                LinkedHashMap::new));
    }

    public GTEFSM fix() {
        Set<GTVState> S;
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta;

        Map<RecVar, GTVState> recvars = new HashMap<>();
        S = this.S.stream().filter(x -> {
            x.recvars.forEach(y -> recvars.put(y, x));
            return !(x instanceof GTVRecVar);
        }).collect(Collectors.toCollection(LinkedHashSet::new));

        delta = this.delta.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> x.getValue().stream().map(y ->
                        y.right instanceof GTVRecVar cast
                        ? new Pair<>(y.left, recvars.get(cast.recvar))
                        : y
                ).collect(Collectors.toCollection(LinkedHashSet::new))
        ));

        return new GTEFSM(S, this.init, this.E, this.A, delta);
    }

    public String toDot() {
        StringBuilder b = new StringBuilder();
        b.append("digraph G {\ncompound = true;\n");
        for (GTVState s : this.S) {
            b.append("\"");
            b.append(s.id);
            b.append("\" [ label=\"");
            b.append(s);
            b.append(":\" ];\n");
        }
        for (Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> x : this.delta.entrySet()) {
            Pair<GTVState, GTVEvent> k = x.getKey();
            for (Pair<GTVAction, GTVState> v : x.getValue()) {
                b.append("\"");
                b.append(k.left.id);
                b.append("\" -> \"");
                b.append(v.right.id);
                b.append("\" [ label=\"");
                b.append(k.right);
                b.append("/");
                b.append(v.left);
                b.append("\" ];\n");
            }
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
