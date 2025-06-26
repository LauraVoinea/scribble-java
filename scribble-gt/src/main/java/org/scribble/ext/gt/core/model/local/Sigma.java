package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.local.action.GTESend;

import java.util.*;
import java.util.stream.Collectors;

public class Sigma {

    public final Map<Role, List<GTESend<DynamicActionKind>>> map;  // Key set is peers (no self)

    public Sigma(Set<Role> peers) {
        this.map = peers.stream().collect(Collectors.toMap(
                x -> x,
                x -> Collections.unmodifiableList(new LinkedList<>())));
    }

    public Sigma(Map<Role, List<GTESend<DynamicActionKind>>> init) {
        this.map = init.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> Collections.unmodifiableList(new LinkedList<>(x.getValue()))));
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
