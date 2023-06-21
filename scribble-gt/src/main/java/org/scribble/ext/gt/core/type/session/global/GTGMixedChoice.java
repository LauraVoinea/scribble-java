package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.local.GTLSystem;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import javax.swing.text.html.Option;
import java.util.*;

public class GTGMixedChoice implements GTGType {

    protected final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final int c;       // Currently assigned by GTGTypeTranslator2
    public final Role other;  // other->observer.L |> observer->other.R
    public final Role observer;  // observer?  "monitor"?
    public final GTGType left;
    public final GTGType right;

    protected GTGMixedChoice(
            int c, GTGType left, GTGType right, Role other, Role observer) {
        this.c = c;
        this.other = other;
        this.observer = observer;
        this.left = left;
        this.right = right;
    }

    /* ... */

    // TODO revisit global props vs. syntactic WF approximations (cf. projection)
    @Override
    public boolean isSinglePointed() {
        Set<Op> ops = this.left.getOps();
        ops.retainAll(this.right.getOps());
        if (!ops.isEmpty()) {
            return false;
        }
        if (!(this.left instanceof GTGInteraction) || !(this.right instanceof GTGInteraction)) {
            return false;
        }
        GTGInteraction left = (GTGInteraction) this.left;
        GTGInteraction right = (GTGInteraction) this.right;
        return left.src.equals(right.dst) && right.dst.equals(this.other)
                && left.dst.equals(right.src) && right.src.equals(this.observer)
                && this.left.isSinglePointed() && this.right.isSinglePointed();
    }

    @Override
    public boolean isGood() {
        return isCoherent();  // TODO redo as full participation
    }

    @Override
    public boolean isCoherent() {
        return this.left.isCoherent() && this.right.isCoherent();
    }

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        Optional<Pair<? extends GTLType, Sigma>> optl = this.left.project(rs, r, c, n);
        Optional<Pair<? extends GTLType, Sigma>> optr = this.right.project(rs, r, c, n);
        if (optl.isEmpty() || optr.isEmpty()) {
            return Optional.empty();
        }
        Sigma s0 = new Sigma(rs);
        Pair<? extends GTLType, Sigma> get_l = optl.get();
        Pair<? extends GTLType, Sigma> get_r = optr.get();
        if (!s0.equals(get_l.right) || !s0.equals(get_r.right)) {
            return Optional.empty();
        }

        /*return !r.equals(this.other) && !r.equals(this.observer)
                ? get_l.left.merge(get_r.left).map(x -> Pair.of(x, s0))  // !!! refactor with GTGInteraction.merge
                : Optional.of(new Pair<>(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));*/
//        HERE HERE XXX could be either white triangle or transparent -merge
//        depending on I / O in full generality ? (white triangle merge
//        only definitely static initial)similarly for black triangle
//
//        cf.regular MPST choice:
//        unlike MC, regular choice not retained as "syntactic context", so
//        the dynamic "merging" between fluctuating I / O doesn 't occur there

        /*if (r.equals(this.other) || r.equals(this.observer)) {
            return Optional.of(Pair.of(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
        } else {
            // TODO conditions? -- abstract global props should be implemented here?

            if (isMergableIOModes(get_l.left, get_r.left)) {
                return get_l.left.merge(get_r.left).map(x -> Pair.of(x, s0));  // !!! refactor with GTGInteraction.merge
            } else {

                // FIXME TODO conditions corresponding to global props?
                // cf. (old) single-pointed
                Set<Op> ops = this.left.getOps();
                ops.retainAll(this.right.getOps());
                if (!ops.isEmpty()) {
                    return Optional.empty();
                }
                if (!(this.left instanceof GTLBranch && this.right instanceof GTLSelect)
                        || !(this.left instanceof GTLSelect && this.right instanceof GTLBranch)) {
                    return Optional.empty();
                }
                if (!getPeer(get_l.left).equals(get_r.left)) {  // CHECKME currently no recursive check (cf. single-pointed, also merge)
                    return Optional.empty();
                }

                return Optional.of(Pair.of(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
            }
        }*/

        if (!r.equals(this.other) && !r.equals(this.observer)) {
            Optional<? extends GTLType> merge = get_l.left.merge(get_r.left);
            if (!merge.isPresent()) {
                return Optional.empty();
            }
        }
        return Optional.of(Pair.of(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
    }

    protected static Role getPeer(GTLType t) {
        if (t instanceof GTLBranch) {
            return ((GTLBranch) t).src;
        } else if (t instanceof GTLSelect) {
            return ((GTLSelect) t).dst;
        } else {
            throw new RuntimeException("Shouldn't get here: " + t);
        }
    }

    protected static boolean isMergableIOModes(GTLType left, GTLType right) {
        IOMode m_left = getMode(left);
        IOMode m_right = getMode(right);
        return m_left == m_right
                && m_left != IOMode.MIXED
                && m_left != IOMode.REC;  // TODO
    }

    // TODO refactor
    protected enum IOMode {
        IN,
        OUT,
        MIXED,
        END,
        REC  // !!! TODO CHECKME merge for rec -- should be "transparent" ?
    }

    protected static IOMode getMode(GTLType t) {
        if (t instanceof GTLBranch) {
            return IOMode.IN;
        } else if (t instanceof GTLSelect) {
            return IOMode.OUT;
        } else if (t instanceof GTLMixedChoice || t instanceof GTLMixedActive) {
            return IOMode.MIXED;
        } else if (t instanceof GTLRecursion) {
            //return getMode(((GTLRecursion) t).body);
            return IOMode.REC;
        } else if (t instanceof GTLEnd) {
            return IOMode.END;
        } else {
            throw new RuntimeException("Shouldn't get here: " + t);
        }
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        return Optional.of(new Theta(cs));
    }

    /* ... */

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        LinkedHashSet<SAction<DynamicActionKind>> res = new LinkedHashSet<>();
        if (theta.map.containsKey(this.c)) {
            Integer m = theta.map.get(this.c);
            res.add(mf.SNewTimeout(this.c, m));
        }
        return res;
    }

    // c, n not checked?
    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {

        if (!(a instanceof GTSNewTimeout)) {  // E.g., (rec) context rule may "attempt"
            return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
        }
        GTSNewTimeout<?> cast = (GTSNewTimeout<?>) a;
        /*Map<Integer, Integer> tmp = new HashMap<>(theta.map);
        tmp.put(nu.c, tmp.get(nu.c) + 1);
        Theta theta1 = new Theta(tmp);*/
        if (cast.c != this.c || cast.n != theta.map.get(this.c)) {
            return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
        }

        Theta theta1 = theta.inc(this.c);
        GTGMixedActive succ = new GTGMixedActive(cast.c, cast.n,  // FIXME use factory?
                this.left, this.right, this.other, this.observer,
                new LinkedHashSet<>(), new LinkedHashSet<>());

        return Either.right(Triple.of(theta1, succ, Tree.of(toStepJudgeString(
                "[Inst]", c, n, theta, this, cast, theta1, succ))));
    }

    /* ... */

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getWeakActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        LinkedHashSet<SAction<DynamicActionKind>> tau = getActs(mf, theta, blocked, c, n);
        if (tau.isEmpty()) {
            return tau;
        } else if (tau.size() > 1) {
            throw new RuntimeException("Shouldn't get in here: " + tau);
        }
        Either<Exception, Triple<Theta, GTGType, Tree<String>>> nu =
                step(theta, tau.iterator().next(), c, n);
        if (nu.isLeft()) {
            return GTUtil.setOf();
        }
        Triple<Theta, GTGType, Tree<String>> get = nu.getRight();  // mixed active
        return get.mid.getWeakActs(mf, get.left, blocked, c, n);
    }

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStep(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        Integer m = theta.map.get(this.c);
        SAction<DynamicActionKind> tau = //...getActs(theta, a, Collections.emptySet(), c, n).iterator().next();
                new GTSNewTimeout(this.c, m);  // TODO factory?
        Either<Exception, Triple<Theta, GTGType, Tree<String>>> weak =
                step(theta, tau, c, n);  // mixed active
        return weak.flatMapRight(x ->
                x.mid.step(x.left, a, c, n).mapRight(y ->
                        Triple.of(y.left, y.mid, Tree.of(
                                toStepJudgeString("[..nu-tau..]", c, n, theta,
                                        this, (GTSAction) a, y.left, y.mid),
                                x.right
                        ))
                )
        );
    }

    /* ... */

    @Override
    public Set<Op> getCommittingTop(Set<Role> com) {
        Set<Op> res = this.left.getCommittingLeft(this.observer, com);
        res.addAll(this.right.getCommittingRight(this.observer, com));
        return res;
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        return getCommittingTop();
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        return getCommittingTop();
    }

    /* Aux */

    @Override
    public GTGMixedChoice subs(Map<RecVar, GTGType> subs) {
        GTGType left = this.left.subs(subs);
        GTGType right = this.right.subs(subs);
        return new GTGMixedChoice(this.c, left, right, this.other, this.observer);
    }

    @Override
    public GTGMixedChoice unfoldAllOnce() {
        return this;
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        Set<Integer> res = new HashSet<>();
        res.add(this.c);
        res.addAll(this.left.getTimeoutIds());
        res.addAll(this.right.getTimeoutIds());
        return res;
    }

    @Override
    public Set<Op> getOps() {
        Set<Op> ops = new HashSet<>(this.left.getOps());
        ops.addAll(this.right.getOps());
        return ops;
    }

    @Override
    public String toString() {
        return ConsoleColors.toMixedChoiceString("(" + this.left + " " + ConsoleColors.WHITE_TRIANGLE
                + this.c + ":" + this.other + "->" + this.observer
                + " " + this.right + ")");
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.MIXED_CHOICE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        hash = 31 * hash + this.other.hashCode();
        hash = 31 * hash + this.observer.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGMixedChoice)) { return false; }
        GTGMixedChoice them = (GTGMixedChoice) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.left.equals(them.left)
                && this.right.equals(them.right)
                && this.other.equals(them.other)
                && this.observer.equals(them.observer);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGMixedChoice;
    }
}
