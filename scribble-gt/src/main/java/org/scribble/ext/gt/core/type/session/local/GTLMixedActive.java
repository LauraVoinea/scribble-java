package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.global.GTGInteraction;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeFactory;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GTLMixedActive implements GTLType {

    protected final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    // TODO Just embed GTMixedChoice?
    public final int c;
    public final GTLType left;
    public final GTLType right;

    public final int n;

    protected GTLMixedActive(int c, int n, GTLType left, GTLType right) {
        this.c = c;
        this.n = n;
        this.left = left;
        this.right = right;
    }

    @Override
    public GTLMixedActive unfoldContext(Map<RecVar, GTLType> env) {
        GTLType left = this.left.unfoldContext(env);
        GTLType right = this.left.unfoldContext(env);
        return this.fact.mixedActive(this.c, this.n, left, right);
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLMixedActive)) {
            return Optional.empty();
        }
        GTLMixedActive cast = (GTLMixedActive) t;
        if (this.c != cast.c || this.n != cast.n
                || !this.left.equals(cast.left) || !this.right.equals(cast.right)) {
            return Optional.empty();
        }
        Optional<? extends GTLType> opt_l = this.left.merge(cast.left);
        Optional<? extends GTLType> opt_r = this.right.merge(cast.right);
        return opt_l.flatMap(x -> opt_r.map(y ->
                this.fact.mixedActive(this.c, this.n, x, y)));
    }

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<Pair<GTLType, Sigma>> step(
            Role self, EAction<DynamicActionKind> a, Sigma sigma, int c, int n) {
        throw new RuntimeException("TODO: " + this);
    }

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            EModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, int c, int n) {  // XXX outer still OK to reduce if inner is fully ended?
        throw new RuntimeException("TODO: " + this);
    }

    /* Aux */

    @Override
    public String toString() {
        return "(" + this.left + " |>"
                + this.c + "," + this.n
                + " " + this.right + ")";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.MIXED_CHOICE_ACTIVE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.n;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLMixedActive)) return false;
        GTLMixedActive them = (GTLMixedActive) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.n == them.n
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLMixedActive;
    }
}
