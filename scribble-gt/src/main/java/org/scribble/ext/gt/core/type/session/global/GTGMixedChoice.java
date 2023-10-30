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
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

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

    /* ... */

    @Override
    public boolean isInitial() {
        return this.left.isInitial() && this.right.isInitial();
    }

    @Override
    public boolean isInitialWellSet(Set<Integer> cs) {
        if (!(this.left instanceof GTGInteraction) || !(this.right instanceof GTGInteraction)) {
            return false;
        }
        if (cs.contains(this.c)) {
            return false;
        }
        Set<Integer> copy = GTUtil.copyOf(cs);
        copy.add(this.c);
        GTGInteraction left = (GTGInteraction) this.left;
        GTGInteraction right = (GTGInteraction) this.right;
        return left.isInitialWellSet(copy) && right.isInitialWellSet(copy)
                && left.getRoles().equals(right.getRoles())  // timeout participation
                && this.other.equals(left.getSender()) && this.other.equals(right.getReceiver())
                && this.observer.equals(left.getReceiver()) && this.observer.equals(right.getSender());
    }

    // Dup with GTGMixedActive  // TODO factor out
    public Set<Role> getIndifferent(Set<Role> top) {
        Set<Role> rs = getRoles();
        Set<Role> copy = GTUtil.copyOf(rs);
        copy.remove(this.other);
        copy.remove(this.observer);
        // !!! conservative? -- CHECKME does that affect safety w.r.t. static awareness?
        return rs.stream().filter(x ->
                        //this.left.projectTop(top, x).equals(this.right.projectTop(top, x)))
                {
                    Optional<Pair<? extends GTLType, Sigma>> o_l = this.left.projectTop(top, x);
                    Optional<Pair<? extends GTLType, Sigma>> o_r = this.right.projectTop(top, x);
                    Optional<Boolean> res = o_l.flatMap(y -> o_r.map(z -> y.left.equals(z.left)));  // !!! only w.r.t. type -- cf. regular/wiggly indiff (non equal queues)
                    return res.isPresent() && res.get();
                })
                .collect(Collectors.toSet());
    }

    @Override
    public Map<Role, Set<Role>> getStrongDeps() {
        Map<Role, Set<Role>> left = this.left.getStrongDeps();
        Map<Role, Set<Role>> right = this.right.getStrongDeps();
        Set<Role> rs = getRoles();
        rs.remove(this.other);
        rs.remove(this.observer);
        Map<Role, Set<Role>> res = GTUtil.mapOf();
        for (Role r : rs) {
            if (!left.containsKey(r) || !right.containsKey(r)) {
                res.put(r, GTUtil.setOf());
                continue;
            }
            Set<Role> tmp = left.get(r);
            tmp.retainAll(right.get(r));
            res.put(r, tmp);
        }
        return res;
    }

    @Override
    public boolean isSingleDecision(Set<Role> top, Theta theta) {
        Map<Role, Set<Role>> right = this.right.getStrongDeps();
        Set<Role> rs = getRoles();
        rs.removeAll(getIndifferent(top));
        rs.remove(this.observer);  // !!! CHECKME
        for (Role r : rs) {

            if (!right.containsKey(r) || !right.get(r).contains(this.observer)) {  // only single-decision -- !!! clear-termination approx by isLeftCommitting
                return false;
            }
        }

        //System.out.println("[Warning] TODO weak-dependencies and clear-termination: " + this);  // cf. isLeftCommitting

        return this.left.isSingleDecision(top, theta) && this.right.isSingleDecision(top, theta);
    }

    @Override
    public boolean isClearTermination() {
        //return isLeftCommitting(GTUtil.setOf(), getRoles());  // n.b., roles(this) -- "outer" roles not involved at all don't matter
        return this.left.isLeftCommittingAux(this.observer, GTUtil.setOf(), getRoles())  // n.b., roles(this) -- "outer" roles not involved at all don't matter
                && this.left.isClearTermination()
                && this.right.isClearTermination();
    }

    @Override
    public boolean isLeftCommitting(Set<Role> com, Set<Role> rem) {
        return this.left.isLeftCommittingAux(this.observer, com, rem)
                && this.left.isClearTermination()
                && this.right.isClearTermination();
    }

    @Override
    public boolean isLeftCommittingAux(Role obs, Set<Role> com, Set<Role> rem) {
        return this.left.isLeftCommittingAux(obs, com, rem)
                && this.right.isLeftCommittingAux(obs, com, rem);
    }

    /* ... */

    @Override
    public boolean isChoicePartip() {
        return this.left.isChoicePartip() && this.right.isChoicePartip();  // XXX CHECKME (cf. merge third parties)
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        // !!! morally can prune if starting from initial
        return this.left.isUniqueInstan(seen) && this.right.isUniqueInstan(seen);
    }

    @Override
    public boolean isAwareCorollary(GTSModelFactory mf, Set<Role> top, Theta theta) {
        // Can morally just return true
        return this.left.isAwareCorollary(mf, top, theta) && this.right.isAwareCorollary(mf, top, theta);
    }

    @Override
    public boolean isCoherent() {
        // Morally can just return true
        return this.left.isCoherent() && this.right.isCoherent();
    }

    /* ... */

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;

        Optional<Pair<? extends GTLType, Sigma>> optl = this.left.project(topPeers, r, c, n);
        Optional<Pair<? extends GTLType, Sigma>> optr = this.right.project(topPeers, r, c, n);
        if (optl.isEmpty() || optr.isEmpty()) { return Optional.empty(); }
        Pair<? extends GTLType, Sigma> get_l = optl.get();
        Pair<? extends GTLType, Sigma> get_r = optr.get();

        Set<Role> top = GTUtil.union(GTUtil.copyOf(topPeers), Set.of(r));
        Set<Role> indiff = getIndifferent(top);
        if (indiff.contains(r)) {
            return get_l.equals(get_r) ? optl : Optional.empty();
        }

        // else r not indiff

        Sigma s0 = new Sigma(topPeers);
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

            // HERE HERE FIXME need to distinguish I/O cases (merge vs. MC)

            if (isMergableIOModes(get_l.left, get_r.left)) {
                Optional<? extends GTLType> merge = get_l.left.merge(get_r.left);
                if (!merge.isPresent()) {
                    return Optional.empty();
                }

            } else {
                // TODO FIXME MC conditions?
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

        // FIXME addRuntimeTestMC(good, bad) -- \nu 2, 2 should not be possible global act, all roles blocked
        LinkedHashSet<SAction<DynamicActionKind>> tmp = get.mid.getWeakActs(mf, get.left, blocked, c, n);
        System.out.println("9999999: " + get.mid + ", " + tmp);
        return tmp;
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
                x.mid.step(x.left, a, c, n).mapRight(y ->  // !!! CHECKME weakStep?  or can MC not be "directly" nested?
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
        /*Set<Op> res = this.left.getCommittingLeft(this.observer, com);
        res.addAll(this.right.getCommittingRight(this.observer, com));*/
        Set<Op> res = this.left.getCommittingLeft(this.observer, GTUtil.setOf());
        res.addAll(this.right.getCommittingRight(this.observer, GTUtil.setOf()));
        return res;
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        //return getCommittingTop();
        return getCommittingTop(com);
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        //return getCommittingTop();
        return getCommittingTop(com);
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> l = this.left.getLabels();
        Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> r = this.right.getLabels();
        Map<Integer, Pair<Set<Op>, Set<Op>>> res = GTUtil.copyOf(l.right);
        if (res.keySet().stream().anyMatch(x -> r.right.containsKey(x))) {
            throw new RuntimeException("Shouldn't get here: " + l + " ,," + r);
        }
        res.putAll(r.right);

        // FIXME merge across nested MCs? cf. TODO merge for MC

        if (res.containsKey(this.c)) {
            throw new RuntimeException("Shouldn't get here: " + l + " ,," + r);
        }

        // CHECKME dropping mergable labs
        Set<Op> l1 = GTUtil.copyOf(l.left);
        Set<Op> r1 = GTUtil.copyOf(r.left);
        l1.removeAll(r.left);
        r1.removeAll(l.left);
        res.put(this.c, Pair.of(l1, r1));
        return Pair.of(GTUtil.setOf(), res);
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
    public Set<Role> getRoles() {
        return GTUtil.union(this.left.getRoles(), this.right.getRoles());
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
    public Set<RecVar> getRecDecls() {
        return GTUtil.union(
                this.left.getRecDecls(),
                this.right.getRecDecls());
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
