package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.util.Pair;

import java.util.*;

public class GTGRecVar implements GTGType {

    public final RecVar var;

    protected GTGRecVar(RecVar var) {
        this.var = var;
    }

    @Override
    public GTGType unfoldContext(Map<RecVar, GTGType> c) {
        return c.containsKey(this.var)
                ? c.get(this.var)
                : this;  // CHECKME
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return Collections.emptySet();
    }

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        return Optional.of(new Pair<>(lf.recVar(this.var), new Sigma()));
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
    public Optional<Pair<Theta, GTGType>> step(Theta theta, SAction a) {
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<SAction> getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked) {
        return new LinkedHashSet<>();
    }

    @Override
    public Set<Op> getOps() {
        return Collections.emptySet();
    }

    /* Aux */

    @Override
    public String toString() {
        return this.var.toString();
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.RECVAR_HASH;
        hash = 31 * hash + this.var.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGRecVar)) return false;
        GTGRecVar them = (GTGRecVar) obj;
        return them.canEquals(this)
                && this.var.equals(them.var);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGRecVar;
    }
}
