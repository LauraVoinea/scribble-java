package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;

// !!! No "fid"
public class GTGEnd implements GTGType {

    public static final GTGEnd END = new GTGEnd();

    protected GTGEnd() { }

    @Override
    public GTGEnd unfoldContext(Map<RecVar, GTGType> c) {
        return this;
    }

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r) {
        return Optional.of(new Pair<>(GTLTypeFactory.FACTORY.end(), new Sigma(rs)));
    }

    @Override
    public boolean isSinglePointed() {
        return true;
    }

    @Override
    public boolean isGood() {
        return true;
    }

    @Override
    public boolean isCoherent() {
        return true;
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return Collections.emptySet();
    }

    @Override
    public Optional<Triple<Theta, GTGType, String>> step(Theta theta, SAction<DynamicActionKind> a) {
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked) {
        return new LinkedHashSet<>();
    }

    @Override
    public Set<Op> getOps() {
        return Collections.emptySet();
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
