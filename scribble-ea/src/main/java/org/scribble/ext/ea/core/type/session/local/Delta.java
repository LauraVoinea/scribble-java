package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.config.EAPSid;
import org.scribble.ext.ea.core.process.EAName;
import org.scribble.ext.ea.core.type.value.EAValType;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class Delta {

    @NotNull public final Map<Pair<EAPSid, Role>, EALType> map;

    public Delta(@NotNull LinkedHashMap<Pair<EAPSid, Role>, EALType> map) {
        this.map = Collections.unmodifiableMap(map.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    /* equals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Delta them = (Delta) o;
        return this.map.equals(them.map);
    }

    @Override
    public int hashCode() {
        int hash = 3121;
        hash = 31 * hash + this.map.hashCode();
        return hash;
    }
}
