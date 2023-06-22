package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.util.Pair;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Sigma {

    public final Map<Role, List<GTESend<DynamicActionKind>>> map;

    public Sigma(Set<Role> rs) {
        this.map = rs.stream().collect(Collectors.toMap(
                x -> x,
                x -> Collections.unmodifiableList(new LinkedList<>())));
    }

    public Sigma(Map<Role, List<GTESend<DynamicActionKind>>> init) {
        this.map = init.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> Collections.unmodifiableList(new LinkedList<>(x.getValue()))));
    }

    public Sigma circ(Sigma x) {
        if (!this.map.keySet().equals(x.map.keySet())) {
            throw new RuntimeException("Sigma circ undefined: "
                    + this.map + " \\circ " + x.map);
        }
        Map<Role, List<GTESend<DynamicActionKind>>> cat =
                this.map.keySet().stream().collect(Collectors.toMap(
                        y -> y,
                        y -> Stream.concat(this.map.get(y).stream(), x.map.get(y).stream())
                                .collect(Collectors.toList())));
        return new Sigma(cat);
    }

    public Sigma gc(Map<Integer, Integer> active) {
        Map<Role, List<GTESend<DynamicActionKind>>> copy = GTUtil.copyOf(this.map);
        for (Role r : copy.keySet()) {
            List<GTESend<DynamicActionKind>> filt = copy.get(r).stream()
                    .filter(x -> active.containsKey(x.c) && active.get(x.c) >= x.n)
                    .collect(Collectors.toList());
            copy.put(r, filt);
        }
        return new Sigma(copy);
    }

    public Sigma gc(Map<Integer, Pair<Set<Op>, Set<Op>>> labs, Map<Pair<Integer, Integer>, Discard> discard) {
        Map<Role, List<GTESend<DynamicActionKind>>> copy = GTUtil.copyOf(this.map);
        Predicate<GTESend<DynamicActionKind>> f = x -> {
            Pair<Integer, Integer> k = Pair.of(x.c, x.n);
            if (!labs.containsKey(k)) { return false; }
            Discard d = discard.get(k);
            if (d == Discard.FULL) { return true; }
            Pair<Set<Op>, Set<Op>> lr = labs.get(x.c);
            return (d == Discard.LEFT && lr.left.contains((Op) x.mid))
                    || (d == Discard.RIGHT && lr.right.contains((Op) x.mid));
        };
        for (Role r : copy.keySet()) {
            List<GTESend<DynamicActionKind>> filt = copy.get(r).stream()
                    .filter(f)
                    .collect(Collectors.toList());
            copy.put(r, filt);
        }
        return new Sigma(copy);
    }

    @Override
    public String toString() {
        return this.map.toString();
    }

    /* hashCode, equals */

    @Override
    public int hashCode() {
        int hash = 33199;
        hash = 31 * hash + this.map.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof Sigma)) { return false; }
        Sigma them = (Sigma) obj;
        return this.map.equals(them.map);
    }
}
