package org.scribble.ext.ea.core.type;

import org.jetbrains.annotations.NotNull;
import org.scribble.ext.ea.core.process.EAName;
import org.scribble.ext.ea.core.process.EAPReturn;
import org.scribble.ext.ea.core.process.EAPTerm;
import org.scribble.ext.ea.core.type.value.EAValType;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class Gamma {

    @NotNull public final Map<EAName, EAValType> map;  // Unmodifiable wrapper of below
    @NotNull protected final LinkedHashMap<EAName, EAValType> _map;  // Backing of above

    public Gamma() {
        this(new LinkedHashMap<>());
    }

    public Gamma(LinkedHashMap<EAName, EAValType> map) {
        this._map = map.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new));
        this.map = Collections.unmodifiableMap(_map);
    }

    @Override
    public String toString() {
        return this.map.toString();
    }

    /* equals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Gamma them = (Gamma) o;
        return this._map.equals(them._map);
    }

    @Override
    public int hashCode() {
        int hash = 3121;
        hash = 31 * hash + this._map.hashCode();
        return hash;
    }
}
