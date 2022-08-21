package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

// !!! No "fid"
public class GTLEnd implements GTLType {

    public static final GTLEnd END = new GTLEnd();

    protected GTLEnd() { }

    @Override
    public Optional<GTLType> step(EAction a) {
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<EAction> getActs(EModelFactory mf, Set<Role> blocked) {
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
        int hash = GTLType.END_HASH;
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLEnd)) return false;
        GTLEnd them = (GTLEnd) obj;
        return them.canEquals(this);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLEnd;
    }
}
