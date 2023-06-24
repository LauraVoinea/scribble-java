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

import java.util.*;

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
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLMixedActive)) {
            return Optional.empty();
        }
        GTLMixedActive cast = (GTLMixedActive) t;
        if (this.c != cast.c || this.n != cast.n
                || !this.left.equals(cast.left) || !this.right.equals(cast.right)) {
            return Optional.empty();
        }

        // !!! CHECKME
        Optional<? extends GTLType> opt_l = this.left.merge(cast.left);
        Optional<? extends GTLType> opt_r = this.right.merge(cast.right);
        return opt_l.flatMap(x -> opt_r.map(y ->
                this.fact.mixedActive(this.c, this.n, x, y)));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {  // XXX outer still OK to reduce if inner is fully ended?

        // TODO remove blocked

        LinkedHashSet<EAction<DynamicActionKind>> aLeft = this.left.getActs(mf, self, blocked, sigma, theta, this.c, this.n);
        LinkedHashSet<EAction<DynamicActionKind>> aRight = this.right.getActs(mf, self, blocked, sigma, theta, this.c, this.n);
        aLeft.addAll(aRight);
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
                this.left.step(com, self, a, sigma, theta, this.c, this.n);
        Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>>> optr =
                this.right.step(com, self, a, sigma, theta, this.c, this.n);
        if (optl.isRight() && optr.isRight()) {
            throw new RuntimeException("TODO: " + optl.getRight() + " ,, " + optr.getRight());

        } else if (optl.isRight()) {
            Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>> get = optl.getRight();
            if (a.isSend()) {
                // [LSnd]
                GTLMixedActive succ = this.fact.mixedActive(
                        this.c, this.n, get.left.fst, this.right);
                return Either.right(Pair.of(
                        Quad.of(succ, get.left.snd, get.left.thrd, Tree.of(
                                toStepJudgeString("[LSnd]", c, n, theta, this,
                                        sigma, (GTEAction) a, get.left.thrd, succ, get.left.snd),
                                get.left.frth
                        )),
                        get.right  // no additional discard
                ));
            } else if (a.isReceive()) {
                // [LRcv1] or [LRcv2]
                GTLType succ;
                String tag;
                Op op = (Op) a.getMid();
                Map<Pair<Integer, Integer>, Discard> discard;
                if (com.contains(op)) {
                    //succ = get.left.fst;
                    succ = this.fact.mixedCommitted(
                            this.c, this.n, get.left.fst, Side.LEFT);
                    tag = "[LRcv1]";
                    discard = GTUtil.copyOf(get.right);
                    discard.put(Pair.of(this.c, this.n), Discard.RIGHT);  // discard RHS
                } else {
                    succ = this.fact.mixedActive(
                            this.c, this.n, get.left.fst, this.right);
                    tag = "[LRcv2]";
                    discard = get.right;  // no additional discard
                }
                return Either.right(Pair.of(
                        Quad.of(succ, get.left.snd, get.left.thrd, Tree.of(
                                toStepJudgeString(tag, c, n, theta, this,
                                        sigma, (GTEAction) a, get.left.thrd, succ, get.left.snd),
                                get.left.frth
                        )),
                        discard
                ));

            } else if (a instanceof GTENewTimeout) {
                // [...ctx...] -- needed ?
                throw new RuntimeException("TODO");
            } else {
                throw new RuntimeException("TODO");
            }

        } else if (optr.isRight()) {
            Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>> get = optr.getRight();
            if (a.isSend()) {
                // [RSnd]
                Map<Pair<Integer, Integer>, Discard> discard = GTUtil.copyOf(get.right);
                discard.put(Pair.of(this.c, this.n), Discard.LEFT);  // discard LHS
                GTLType succ = this.fact.mixedCommitted(this.c, this.n, get.left.fst, Side.RIGHT);
                return Either.right(Pair.of(
                        Quad.of(succ, get.left.snd, get.left.thrd, Tree.of(
                                toStepJudgeString("[RSnd]", c, n, theta, this,
                                        sigma, (GTEAction) a, get.left.thrd, succ, get.left.snd),
                                get.left.frth
                        )),
                        discard
                ));
            } else if (a.isReceive()) {
                // [RRcv] -- same as RSnd?
                Map<Pair<Integer, Integer>, Discard> discard = GTUtil.copyOf(get.right);
                discard.put(Pair.of(this.c, this.n), Discard.LEFT);  // discard LHS
                GTLType succ = this.fact.mixedCommitted(this.c, this.n, get.left.fst, Side.RIGHT);
                return Either.right(Pair.of(
                        Quad.of(succ, get.left.snd, get.left.thrd, Tree.of(
                                toStepJudgeString("[RRcv]", c, n, theta, this,
                                        sigma, (GTEAction) a, get.left.thrd, succ, get.left.snd),
                                get.left.frth
                        )),
                        discard
                ));
            } else if (a instanceof GTENewTimeout) {
                // [...ctx...] -- needed ?
                throw new RuntimeException("TODO");
            } else {
                throw new RuntimeException("TODO");
            }

        } else {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
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
        Map<Integer, Integer> left = this.left.getActive(theta);
        Map<Integer, Integer> right = this.right.getActive(theta);
        Map<Integer, Integer> min = min(left, right);
        if (!min.containsKey(this.c) || this.n < min.get(this.c)) {
            min.put(this.c, this.n);
        }
        return min;
    }

    // cf. GTLMixedActive.projectTheta -> max
    public static Map<Integer, Integer> min(
            Map<Integer, Integer> t1, Map<Integer, Integer> t2) {
        Map<Integer, Integer> map = GTUtil.copyOf(t1);
        for (Map.Entry<Integer, Integer> e : t2.entrySet()) {
            int k = e.getKey();
            int v = e.getValue();
            if (!map.containsKey(k) || v < map.get(k)) {
                map.put(k, v);
            }
        }
        return map;
    }

    @Override
    public GTLType subs(RecVar rv, GTLType t) {
        GTLType left = this.left.subs(rv, t);
        GTLType right = this.right.subs(rv, t);
        return this.fact.mixedActive(this.c, this.n, left, right);
    }

    @Override
    public GTLMixedActive unfoldAllOnce() {
        return this;
    }


    @Override
    public String toString() {
        return "(" + this.left + " " + ConsoleColors.BLACK_TRIANGLE
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
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLMixedActive)) { return false; }
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
