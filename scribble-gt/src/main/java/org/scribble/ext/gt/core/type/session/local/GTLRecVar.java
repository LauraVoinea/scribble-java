package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class GTLRecVar implements GTLType {

    public final RecVar var;

    protected GTLRecVar(RecVar var) {
        this.var = var;
    }

    @Override
    public GTLType unfoldContext(Map<RecVar, GTLType> env) {
        return env.containsKey(this.var)
                ? env.get(this.var)
                : this;  // CHECKME
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return this.equals(t) ? Optional.of(this) : Optional.empty();
    }

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
        return this.var.toString();
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.RECVAR_HASH;
        hash = 31 * hash + this.var.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLRecVar)) return false;
        GTLRecVar them = (GTLRecVar) obj;
        return them.canEquals(this)
                && this.var.equals(them.var);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLRecVar;
    }
}
