package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.global.GTGMixedChoice;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// HERE extend ANTLR -- copy frontend stuff from scrib-assrt
public class GTLMixedChoice implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final int c;
    public final GTLType left;
    public final GTLType right;

    protected GTLMixedChoice(
            int c, GTLType left, GTLType right) {
        this.c = c;
        this.left = left;
        this.right = right;
    }

    @Override
    public GTLMixedChoice unfoldContext(Map<RecVar, GTLType> env) {
        GTLType left = this.left.unfoldContext(env);
        GTLType right = this.right.unfoldContext(env);
        return this.fact.mixedChoice(this.c, left, right);
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLMixedChoice)) {
            return Optional.empty();
        }
        GTLMixedChoice cast = (GTLMixedChoice) t;
        if (this.c != cast.c || !this.left.equals(cast.left)
                || !this.right.equals(cast.right)) {
            return Optional.empty();
        }
        Optional<? extends GTLType> opt_l = this.left.merge(cast.left);
        Optional<? extends GTLType> opt_r = this.right.merge(cast.right);
        return opt_l.flatMap(x -> opt_r.map(y -> this.fact.mixedChoice(this.c, x, y)));
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
            EModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, int c, int n) {
        throw new RuntimeException("TODO: " + this);
    }

    /* Aux */

    @Override
    public String toString() {
        return ConsoleColors.toMixedChoiceString(this.left + " |>" + this.c + " " + this.right);
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.MIXED_CHOICE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLMixedChoice)) return false;
        GTLMixedChoice them = (GTLMixedChoice) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLMixedChoice;
    }
}
