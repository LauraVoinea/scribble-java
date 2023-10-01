package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.MActionBase;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Discard;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.GTEModelFactoryImpl;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTENewTimeout;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

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

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c1, int n1) {
        LinkedHashSet<EAction<DynamicActionKind>> res = new LinkedHashSet<>();
        if (theta.map.containsKey(this.c)) {
            Integer m = theta.map.get(this.c);
            res.add(mf.DynamicGTENewTimeout(this.c, m));
        }
        return res;
    }

    // Pre: a in getActs
    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {

        if (!(a instanceof GTENewTimeout)) {  // E.g., (rec) context rule may "attempt"
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        GTENewTimeout<?> cast = (GTENewTimeout<?>) a;
        if (cast.c != this.c || cast.n != theta.map.get(this.c)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }

        Theta theta1 = theta.inc(this.c);
        GTLMixedActive succ = new GTLMixedActive(cast.c, cast.n, this.left, this.right);  // FIXME use factory?
        return Either.right(Pair.of(Quad.of(succ, sigma, theta1, Tree.of(
                        toStepJudgeString("[..NewChan..]", c, n, theta, this,
                                sigma, (GTEAction) a, theta1, succ, sigma)
                )),

                // top-level c must not clash with any other tid (cf. n-1)
                GTUtil.mapOf(Pair.of(cast.c, cast.n - 1),  // cf. GTLType.n_INIT = 1  (first "next" is 1)
                        Discard.FULL)
        ));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getWeakActs(
            GTEModelFactory mf, Set<Op> com, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        LinkedHashSet<EAction<DynamicActionKind>> tau = getActs(mf, self, blocked, sigma, theta, c, n);
        if (tau.isEmpty()) {
            return tau;
        } else if (tau.size() > 1) {
            throw new RuntimeException("Shouldn't get in here: " + tau);
        }
        Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
                Map<Pair<Integer, Integer>, Discard>>> step =
                step(com, self, tau.iterator().next(), sigma, theta, c, n);
        if (step.isLeft()) {
            return GTUtil.setOf();
        }
        Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>> get =
                step.getRight();  // mixed active
        return get.left.fst.getWeakActs(mf, com, self, blocked, get.left.snd, get.left.thrd, c, n);
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> weakStep(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        Integer m = theta.map.get(this.c);
        EAction<DynamicActionKind> tau = //...getActs(theta, a, Collections.emptySet(), c, n).iterator().next();
                new GTENewTimeout<>(MActionBase.DYNAMIC_ID, GTEModelFactoryImpl.FACTORY, this.c, m);  // FIXME factory
        Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>>> weak =
                step(com, self, tau, sigma, theta, c, n);  // MixedActive
        return weak.flatMapRight(x ->
                x.left.fst.step(com, self, a, x.left.snd, x.left.thrd, c, n).mapRight(y -> { // MixedActive, so weak redundant
                            Map<Pair<Integer, Integer>, Discard> d1 = GTUtil.copyOf(x.right);
                            d1.putAll(y.right);
                            return Pair.of(
                                    Quad.of(y.left.fst, y.left.snd, y.left.thrd, Tree.of(
                                            toStepJudgeString("[..nu-tau..]", c, n, theta,
                                                    this, sigma, (GTEAction) a, y.left.thrd, y.left.fst, y.left.snd),
                                            x.left.frth
                                    )),
                                    d1
                            );
                        }
                ));
    }

    /* Aux */

    @Override
    public Map<Integer, Integer> getActive(Theta theta) {
        if (!theta.map.containsKey(this.c)) {
            throw new RuntimeException("Shouldn't get here: " + this);
        }
        return GTUtil.mapOf(this.c, theta.map.get(this.c));
    }

    @Override
    public GTLMixedChoice subs(RecVar rv, GTLType t) {
        GTLType left = this.left.subs(rv, t);
        GTLType right = this.right.subs(rv, t);
        return this.fact.mixedChoice(this.c, left, right);
    }

    @Override
    public GTLMixedChoice unfoldAllOnce() {
        return this;
    }

    @Override
    public String toString() {
        return ConsoleColors.toMixedChoiceString(this.left + " " + ConsoleColors.WHITE_TRIANGLE
                + this.c + " " + this.right);
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
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLMixedChoice)) { return false; }
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
