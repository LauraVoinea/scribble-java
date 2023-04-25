package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.ModelFactory;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.global.GTGEnd;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.util.Pair;

import java.util.*;

// !!! No "fid"
public class GTLEnd implements GTLType {

    public static final GTLEnd END = new GTLEnd();

    protected GTLEnd() { }

    @Override
    public GTLEnd unfoldContext(Map<RecVar, GTLType> env) {
        return this;
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return t.equals(END) ? Optional.of(END) : Optional.empty();
    }

    @Override
    public Optional<Pair<GTLType, Sigma>> step(
            Role self, EAction<DynamicActionKind> a, Sigma sigma, int c, int n) {
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            EModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, int c, int n) {
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
