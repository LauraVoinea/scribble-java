package org.scribble.ext.ea.core.type.session.local;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.ea.core.type.value.EAVType;
import org.scribble.util.Pair;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public abstract class EALTypeIOBase implements EALType {

    @NotNull
    public final Role peer;
    @NotNull
    public final Map<Op, Pair<EAVType, EALType>> cases;

    protected EALTypeIOBase(@NotNull Role peer,
                            @NotNull LinkedHashMap<Op, Pair<EAVType, EALType>> cases) {
        this.peer = peer;
        if (cases.isEmpty()) {
            throw new RuntimeException("Invalid empty cases");
        }
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public EALType unfoldAllOnce() {
        return this;
    }

    @Deprecated
    public LinkedHashMap<Op, Pair<EAVType, EALType>> unfoldCases(
            RecVar rvar, EALType t) {
        return this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> {
                            Pair<EAVType, EALType> v = x.getValue();
                            return new Pair(v.left, v.right.unfold(rvar, t));
                        },
                        (x, y) -> x, LinkedHashMap::new
                ));
    }

    /* Aux */

    public boolean isOut() {
        return false;
    }

    public boolean isIn() {
        return false;
    }

    public abstract String symbol();

    @Override
    public String toString() {
        return this.peer + symbol() + "{"
                + this.cases.entrySet().stream().map(x -> {
            Op k = x.getKey();
            Pair<EAVType, EALType> v = x.getValue();
            return k + "(" + v.left + ")" + "." + v.right;
        }).collect(Collectors.joining(", "))
                + "}";
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EALTypeIOBase them = (EALTypeIOBase) o;
        return them.canEquals(this)
                && this.peer.equals(them.peer)
                && this.cases.equals(them.cases);
    }

    @Override
    public int hashCode() {
        int hash = EALType.IO_HASH;
        hash = 31 * hash + this.peer.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }
}
