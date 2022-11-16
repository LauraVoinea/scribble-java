package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.core.type.name.RecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;
import org.scribble.ext.assrt.util.AssrtUtil;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class AssrtRho {

    // RecVar is the token value, AssrtLRecVar is the type
    public final Map<RecVar, Pair<AssrtLambda, AssrtLType>> map;

    public AssrtRho() {
       this(new LinkedHashMap<>());
    }

    public AssrtRho(LinkedHashMap<RecVar, Pair<AssrtLambda, AssrtLType>> map) {
        this.map = Collections.unmodifiableMap(new LinkedHashMap<>(map));
    }

    @Override
    public String toString() {
        return "{" +
                this.map.entrySet().stream()
                        .map(x -> x.getKey() + "=" + AssrtUtil.pairToString(x.getValue()))
                        .collect(Collectors.joining(", ")) +
                "}";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof AssrtRho)) {
            return false;
        }
        return this.map.equals(((AssrtRho) obj).map);
    }

    @Override
    public int hashCode() {
        int hash = 7643;
        hash = 31*hash + this.map.hashCode();
        return hash;
    }
}
