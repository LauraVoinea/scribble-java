package org.scribble.ext.gt.core.type.session.global;

import org.antlr.runtime.tree.CommonTree;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.kind.Global;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.SType;
import org.scribble.core.type.session.global.GSeq;
import org.scribble.core.visit.STypeAgg;
import org.scribble.core.visit.STypeAggNoThrow;
import org.scribble.ext.gt.core.type.session.local.GTLEnd;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.util.ScribException;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;

// !!! No "fid"
public class GTGEnd implements GTGType {

    public static final GTGEnd END = new GTGEnd();

    protected GTGEnd() { }

    @Override
    public Optional<GTLEnd> project(Role r) {
        return Optional.of(GTLTypeFactory.FACTORY.end());
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
    public Optional<GTGType> step(SAction a) {
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<SAction> getActs(SModelFactory mf, Set<Role> blocked) {
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
