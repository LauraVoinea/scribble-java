package org.scribble.ext.ea.core.process;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class EAHandlers implements EATerm {

    @NotNull public final Map<Op, EAExpr> cases;

    public EAHandlers(@NotNull LinkedHashMap<Op, EAExpr> cases) {
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    /* equals/canEquals, hashCode */

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        EAHandlers eaVar = (EAHandlers) o;
        return eaVar.canEquals(this) && this.cases.equals(eaVar.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof EAHandlers;
    }

    @Override
    public int hashCode() {
        int hash = EATerm.HANDLERS;
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }
}
