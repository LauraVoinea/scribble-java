package org.scribble.ext.assrt.core.type.formal.local;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.Op;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.util.Pair;

import javax.swing.text.html.Option;
import java.lang.management.OperatingSystemMXBean;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;

public class AssrtLambda {
    public final Map<AssrtVar, Pair<Multiplicity, DataName>> map;

    public AssrtLambda(LinkedHashMap<AssrtVar, Pair<Multiplicity, DataName>> map) {
        this.map = Collections.unmodifiableMap(new LinkedHashMap<>(map));
    }

    // TODO refactor
    public boolean canAdd(AssrtVar v, Multiplicity theta, DataName t) {
        if (theta == Multiplicity.ZERO || theta == Multiplicity.HAT) {
            if (!this.map.containsKey(v)) {
                return true;
            } else {
                Pair<Multiplicity, DataName> tmp = this.map.get(v);
                return tmp.equals(new Pair<>(theta, t));
            }
        } else if (theta == Multiplicity.OMEGA) {
            if (!this.map.containsKey(v)) {
                return true;
            } else {
                Pair<Multiplicity, DataName> tmp = this.map.get(v);
                if (tmp.equals(new Pair<>(Multiplicity.OMEGA, t))) {
                    return true;
                } else {
                    if (tmp.equals(new Pair<>(Multiplicity.HAT, t))) { // N.B. HAT
                        return true;
                    } else {
                        return false;
                    }
                }
            }
        } else {
            return false;
        }
    }

    public Optional<AssrtLambda> add(AssrtVar v, Multiplicity theta, DataName t) {
        if (theta == Multiplicity.ZERO || theta == Multiplicity.HAT) {
            if (!this.map.containsKey(v)) {
                LinkedHashMap<AssrtVar, Pair<Multiplicity, DataName>> tmp =
                        new LinkedHashMap<>(this.map);
                tmp.put(v, new Pair<>(theta, t));
                return Optional.of(new AssrtLambda(tmp));
            } else {
                Pair<Multiplicity, DataName> tmp = this.map.get(v);
                return tmp.equals(new Pair<>(theta, t))
                        ? Optional.of(this)
                        : Optional.empty();
            }
        } else if (theta == Multiplicity.OMEGA) {
            if (!this.map.containsKey(v)) {
                LinkedHashMap<AssrtVar, Pair<Multiplicity, DataName>> tmp =
                        new LinkedHashMap<>(this.map);
                tmp.put(v, new Pair<>(Multiplicity.HAT, t));
                return Optional.of(new AssrtLambda(tmp));
            } else {
                Pair<Multiplicity, DataName> tmp = this.map.get(v);
                if (tmp.equals(new Pair<>(Multiplicity.OMEGA, t))) {
                    return Optional.of(this);
                } else {
                    if (tmp.equals(new Pair<>(Multiplicity.HAT, t))) { // N.B. HAT
                        LinkedHashMap<AssrtVar, Pair<Multiplicity, DataName>> map1 = new LinkedHashMap<>(this.map);
                        map1.put(v, new Pair<>(Multiplicity.OMEGA, t));  // Replaces prev HAT pair
                        return Optional.of(new AssrtLambda(map1));
                    } else {
                        return Optional.empty();
                    }
                }
            }
        } else {
            return Optional.empty();
        }
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
        if (!(obj instanceof AssrtLambda)) {
            return false;
        }
        return this.map.equals(((AssrtLambda) obj).map);
    }

    @Override
    public int hashCode() {
        int hash = 7639;
        hash = 31*hash + this.map.hashCode();
        return hash;
    }
}
