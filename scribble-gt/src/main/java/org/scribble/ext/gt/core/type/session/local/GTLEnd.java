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
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

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
    public Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
    }

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
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
