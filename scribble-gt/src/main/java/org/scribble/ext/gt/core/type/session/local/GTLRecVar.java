package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.Quad;
import org.scribble.ext.gt.util.Tree;

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
    public Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
    }

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        throw new RuntimeException("Unsupported operation: " + this);
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
