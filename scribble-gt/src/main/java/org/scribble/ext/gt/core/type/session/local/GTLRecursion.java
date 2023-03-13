package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.util.ConsoleColors;

import java.util.*;

public class GTLRecursion implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final RecVar var;
    public final GTLType body;

    protected GTLRecursion(RecVar var, GTLType body) {
        this.var = var;
        this.body = body;
    }

    @Override
    public GTLType unfoldContext(Map<RecVar, GTLType> env) {
        if (env.containsKey(this.var)) {
            return this;
        }
        Map<RecVar, GTLType> nested = new HashMap<>(env);
        nested.put(this.var, this);
        return this.body.unfoldContext(nested);
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLRecursion)) {
            return Optional.empty();
        }
        GTLRecursion cast = (GTLRecursion) t;
        if (this.var.equals(cast.var)) {
            Optional<? extends GTLType> merge = this.body.merge(cast.body);
            return merge.map(x -> this.fact.recursion(this.var, x));
        } else {
            return Optional.empty();
        }
    }

    @Override
    public Optional<GTLType> step(EAction a) {
        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public LinkedHashSet<EAction> getActs(EModelFactory mf, Set<Role> blocked) {
        return this.body.getActs(mf, blocked);
    }

    /* Aux */

    @Override
    public String toString() {
        return ConsoleColors.toRecString("mu " + this.var + "." + this.body);
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.REC_HASH;
        hash = 31 * hash + this.var.hashCode();
        hash = 31 * hash + this.body.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLRecursion)) return false;
        GTLRecursion them = (GTLRecursion) obj;
        return them.canEquals(this)
                && this.var.equals(them.var)
                && this.body.equals(them.body);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLRecursion;
    }
}
