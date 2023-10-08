package org.scribble.ext.gt.core.model.global;

import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.local.GTLType;

import java.util.*;
import java.util.stream.Collectors;

public class Theta {

    public final Map<Integer, Integer> map;  // "next" -- the id for the *NEXT* active instance, cf. GTLType.n_INIT

    public Theta(Set<Integer> init) {
        this.map = init.stream().collect(Collectors.toMap(x -> x, x -> GTLType.n_INIT));
    }

    public Theta(Map<Integer, Integer> map) {
        this.map = Collections.unmodifiableMap(new HashMap<>(map));
    }

    // Pre: this.map.keySet().contains(c)
    public final Theta inc(int c) {
        Map<Integer, Integer> tmp = new HashMap<>(this.map);
        tmp.put(c, tmp.get(c) + 1);
        return new Theta(tmp);
    }

    // HERE HERE global vs. local modelling of Theta (sync vs async w.r.t Theta) -- projection of global Theta to local Thetas
    public Theta project(Role r) {
        throw new RuntimeException("TODO");  // Currently GTGType.projectTheta
    }

    @Override
    public String toString() {
        return this.map.toString();
    }

    /* hashCode, equals */

    @Override
    public int hashCode() {
        int hash = 33191;
        hash = 31 * hash + this.map.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof Theta)) { return false; }
        Theta them = (Theta) obj;
        return this.map.equals(them.map);
    }

    /* ... */

    public static Optional<Theta> max(Theta t1, Theta t2) {
        if (!t1.map.keySet().equals(t2.map.keySet())) {
            Optional.empty();
        }
        Map<Integer, Integer> map = new HashMap<>(t1.map);
        for (Map.Entry<Integer, Integer> e : t2.map.entrySet()) {
            int k = e.getKey();
            int v = e.getValue();
            if (v > map.get(k)) {
                map.put(k, v);
            }
        }
        return Optional.of(new Theta(map));
    }
}
