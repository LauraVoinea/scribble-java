package org.scribble.ext.assrt.core.type.formal.global;
import org.scribble.core.type.name.DataName;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.assrt.core.type.formal.Multiplicity;
import org.scribble.ext.assrt.core.type.formula.AssrtBFormula;
import org.scribble.ext.assrt.core.type.name.AssrtVar;
import org.scribble.ext.assrt.util.AssrtUtil;
import org.scribble.ext.assrt.util.Quadple;
import org.scribble.ext.assrt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class AssrtPhi {
    //public final Map<RecVar, Quadple<AssrtVar, Set<Role>, DataName, AssrtBFormula>> map;
    public final Map<RecVar, Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>>> map;

    public AssrtPhi() {
        this(new LinkedHashMap<>());
    }

    public AssrtPhi(LinkedHashMap<RecVar, Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>>> map) {
        //this.map = Collections.unmodifiableMap(new LinkedHashMap<>(map));
        this.map = Collections.unmodifiableMap(map.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> Collections.unmodifiableMap(x.getValue().entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        y -> {
                            Triple<Set<Role>, DataName, AssrtBFormula> tmp = y.getValue();
                            return new Triple<>(new HashSet(tmp.left), tmp.middle, tmp.right);
                        },
                        (z1, z2) -> null,
                        LinkedHashMap::new
                ))),
                (x, y) -> null,
                LinkedHashMap::new
        )));
    }

    // Disjoint union
    public Optional<AssrtPhi> comma(RecVar rv, Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>> vars) {
        if (this.map.containsKey(rv)) {
            return Optional.empty();
        } else {
            LinkedHashMap<RecVar, Map<AssrtVar, Triple<Set<Role>, DataName, AssrtBFormula>>> tmp = new LinkedHashMap<>(this.map);
            tmp.put(rv, vars);
            return Optional.of(new AssrtPhi(tmp));  // Does def copy (cf. above not defensive)
        }
    }

    @Override
    public String toString() {
        return "{" +
                this.map.entrySet().stream()
                        .map(x -> x.getKey() + "=["+ x.getValue().entrySet().stream()
                                .map(y -> y.getKey() + "=" + AssrtUtil.tripleToString(y.getValue()))
                                .collect(Collectors.joining(", ")) + "]")
                        .collect(Collectors.joining(", ")) +
                "}";
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (!(obj instanceof AssrtPhi)) {
            return false;
        }
        return this.map.equals(((AssrtPhi) obj).map);
    }

    @Override
    public int hashCode() {
        int hash = 7639;
        hash = 31*hash + this.map.hashCode();
        return hash;
    }
}
