package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Discard;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

// Dup from GTLMixedActive
public class GTLMixedCommitted implements GTLType {

    protected final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    // TODO Just embed GTMixedChoice?
    public final int c;
    public final GTLType type;
    public final Side side;

    public final int n;

    protected GTLMixedCommitted(int c, int n, GTLType type, Side side) {
        this.c = c;
        this.n = n;
        this.type = type;
        this.side = side;
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLMixedCommitted)) {
            return Optional.empty();
        }
        GTLMixedCommitted cast = (GTLMixedCommitted) t;
        if (this.c != cast.c || this.n != cast.n
                || !this.type.equals(cast.type)) {
            return Optional.empty();
        }
        Optional<? extends GTLType> opt_l = this.type.merge(cast.type);
        return opt_l.map(x -> this.fact.mixedCommitted(this.c, this.n, x, this.side));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {  // XXX outer still OK to reduce if inner is fully ended?

        // TODO remove blocked

        LinkedHashSet<EAction<DynamicActionKind>> aLeft = this.type.getActs(mf, self, blocked, sigma, theta, this.c, this.n);
        return aLeft;
    }

    // Pre: a in getActs
    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {

        /*if (!a.peer.equals(self)) {  // ...cf. "context" rule?
            return Optional.empty();
        }*/
        Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>>> optl =
                this.type.step(com, self, a, sigma, theta, this.c, this.n);

        return optl.mapRight(x -> {
            Quad<GTLType, Sigma, Theta, Tree<String>> step = x.left;
            GTLMixedCommitted succ = this.fact.mixedCommitted(
                    this.c, this.n, step.fst, this.side);
            return Pair.of(
                    Quad.of(succ, step.snd, step.thrd, Tree.of(
                            toStepJudgeString("[..LCommitted..]", c, n, theta, this,
                                    sigma, (GTEAction) a, step.thrd, succ, step.snd),
                            step.frth
                    )),
                    x.right  // no additional discard
            );
        });
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getWeakActs(
            GTEModelFactory mf, Set<Op> com, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        return getActs(mf, self, blocked, sigma, theta, c, n);
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> weakStep(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        return step(com, self, a, sigma, theta, c, n);
    }

    /* Aux */

    @Override
    public Map<Integer, Integer> getActive(Theta theta) {
        Map<Integer, Integer> min = this.type.getActive(theta);
        if (!min.containsKey(this.c) || this.n < min.get(this.c)) {
            min.put(this.c, this.n);
        }
        return min;
    }

    @Override
    public GTLType subs(RecVar rv, GTLType t) {
        GTLType left = this.type.subs(rv, t);
        GTLType right = this.type.subs(rv, t);
        return this.fact.mixedActive(this.c, this.n, left, right);
    }

    @Override
    public GTLMixedCommitted unfoldAllOnce() {
        return this;
    }


    @Override
    public String toString() {
        String triangle = "" + ConsoleColors.BLACK_TRIANGLE + this.c + "," + this.n;
        return this.side == Side.LEFT
                ? "(" + this.type + " " + triangle + " " + ConsoleColors.BULLET + ")"
                : "(" + ConsoleColors.BULLET + " " + triangle + " " + this.type + ")";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.MIXED_CHOICE_ACTIVE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.n;
        hash = 31 * hash + this.type.hashCode();
        hash = 31 * hash + this.side.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLMixedCommitted)) {
            return false;
        }
        GTLMixedCommitted them = (GTLMixedCommitted) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.n == them.n
                && this.type.equals(them.type)
                && this.side.equals(them.side);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLMixedCommitted;
    }
}
