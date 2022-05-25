package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.Map;
import java.util.stream.Collectors;

public class GTGChoice implements GTGType {

    public final Role src;
    public final Role dst;
    public final Map<Op, GTGType> cases;

    protected GTGChoice(Role src, Role dst, Map<Op, GTGType> cases) {
        this.src = src;
        this.dst = dst;
        this.cases = cases.entrySet().stream().collect(
                Collectors.toUnmodifiableMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /* Aux */

    @Override
    public String toString() {
        return this.src + "->" + this.dst
                + "{" + this.cases.entrySet().stream()
                .map(e -> e.getKey() + "." + e.getValue())
                .collect(Collectors.joining(", ")) + "}";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.GCHOICE;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGChoice)) return false;
        GTGChoice them = (GTGChoice) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.dst.equals(them.dst)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGChoice;
    }
}
