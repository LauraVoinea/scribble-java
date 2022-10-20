package org.scribble.ext.assrt.core.type.formal.local;

import org.scribble.ext.assrt.core.type.session.local.AssrtLRecVar;
import org.scribble.ext.assrt.core.type.session.local.AssrtLType;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class Rho {
    public final Map<AssrtLRecVar, Pair<AssrtLambda, AssrtLType>> map;

    public Rho(LinkedHashMap<AssrtLRecVar, Pair<AssrtLambda, AssrtLType>> map) {
        this.map = Collections.unmodifiableMap(new LinkedHashMap<>(map));
    }

    @Override
    public String toString() {
        return this.map.toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof Rho)) {
            return false;
        }
        return this.map.equals(((Rho) obj).map);
    }

    @Override
    public int hashCode() {
        int hash = 7643;
        hash = 31*hash + this.map.hashCode();
        return hash;
    }
}
