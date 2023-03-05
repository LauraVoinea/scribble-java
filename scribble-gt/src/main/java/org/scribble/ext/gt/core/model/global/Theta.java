package org.scribble.ext.gt.core.model.global;

import org.scribble.core.type.name.Role;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Theta {

    public final Map<Integer, Integer> map;

    public Theta(Set<Integer> init) {
        this.map = init.stream().collect(Collectors.toMap(x -> x, x -> 1));
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
        throw new RuntimeException("TODO");
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
        if (this == obj) return true;
        if (obj == null || !(obj instanceof Theta)) return false;
        Theta them = (Theta) obj;
        return this.map.equals(them.map);
    }
}
