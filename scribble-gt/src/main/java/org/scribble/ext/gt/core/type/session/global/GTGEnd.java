package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Role;

import java.util.*;

// !!! No "fid"
public class GTGEnd implements GTGType {

    public static final GTGEnd END = new GTGEnd();

    protected GTGEnd() { }

    @Override
    public Optional<GTGType> step(SAction a) {
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<SAction> getActs(SModelFactory mf, Set<Role> blocked) {
        return new LinkedHashSet<>();
    }

    /* Aux */

    @Override
    public String toString() {
        return "end";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.END_HASH;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGEnd)) return false;
        GTGEnd them = (GTGEnd) obj;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGEnd;
    }
}
