package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.GTSModelFactoryImpl;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GTGMixedActive implements GTGType {

    protected final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    // TODO Just embed GTMixedChoice?
    public final int c;
    public final GTGType left;
    public final GTGType right;
    public final Role other;  // other->observer.L |> observer->other.R
    public final Role observer;  // observer?  "monitor"?

    public final int n;
    public final Set<Role> committedLeft;
    public final Set<Role> committedRight;

    protected GTGMixedActive(int c, int n,
                             GTGType left, GTGType right, Role other, Role observer,
                             LinkedHashSet<Role> committedLeft, LinkedHashSet<Role> committedRight) {
        this.c = c;
        this.n = n;
        this.left = left;
        this.right = right;
        this.other = other;
        this.observer = observer;
        this.committedLeft = Collections.unmodifiableSet(
                new LinkedHashSet<>(committedLeft));
        this.committedRight = Collections.unmodifiableSet(
                new LinkedHashSet<>(committedRight));
    }

    /* ... */

    // Does Sigma.circ -- cf. GTGInteraction
    public static Optional<Pair<? extends GTLType, Sigma>> mergePair(
            Optional<Pair<? extends GTLType, Sigma>> left,
            Optional<Pair<? extends GTLType, Sigma>> right) {
        Optional<? extends GTLType> merge = GTGInteraction.merge(left.map(x -> x.left), right.map(x -> x.left));
        Optional<Sigma> sigma = left.flatMap(x -> right.map(y -> x.right.circ(y.right)));
        return merge.flatMap(x -> sigma.map(y -> new Pair<>(x, y)));  // nested `map` OK, result should be empty only when Opt is empty
    }

    @Override
    public boolean isSinglePointed() {
        if (!this.committedLeft.isEmpty() || !this.committedRight.isEmpty()) {
            throw new RuntimeException();
        }
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
        return (this.committedLeft.isEmpty() || this.committedRight.isEmpty())
                && this.left.isGood() && this.right.isGood();
    }

    /* ... */

    @Override
    public boolean isInitial() {
        return false;
    }

    @Override
    public boolean isInitialWellSet(Set<Integer> cs) {
        return false;
    }

    // Dup from GTGMixedChoice
    public Set<Role> getIndifferent() {
        Set<Role> rs = getRoles();
        Set<Role> copy = GTUtil.copyOf(rs);
        copy.remove(this.other);
        copy.remove(this.observer);
        // !!! conservative? -- CHECKME does that affect safety w.r.t. static awareness?
        return rs.stream().filter(x ->
                        this.left.projectTop(rs, x).equals(this.right.projectTop(rs, x)))
                .collect(Collectors.toSet());
    }

    @Override
    public Map<Role, Set<Role>> getStrongDeps() {
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    @Override
    public boolean isInitialAware(Theta theta) {
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    @Override
    public boolean isLeftCommittingTop() {
        throw new RuntimeException("Shouldn't get in here: ");
    }

    @Override
    public boolean isLeftCommitting(Set<Role> com, Set<Role> rem) {
        /*Set<Role> rs = GTUtil.copyOf(getRoles());
        rs.removeAll(this.committedLeft);
        return this.left.isLeftCommitting(this.observer, com, rs);*/
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    @Override
    public boolean isLeftCommittingAux(Role obs, Set<Role> com, Set<Role> rem) {
        /*Set<Role> rs = GTUtil.copyOf(getRoles());
        rs.removeAll(this.committedLeft);
        return this.left.isLeftCommitting(this.observer, com, rs)
                && this.left.isLeftCommitting(obs, com, rem)
                && this.right.isLeftCommitting(obs, com, rem);*/
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    /* ... */

    @Override
    public boolean isChoicePartip() {
        return this.left.isChoicePartip() && this.right.isChoicePartip();  // XXX CHECKME (cf. merge third parties)
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        Pair<Integer, Integer> p = Pair.of(this.c, this.n);
        if (seen.contains(p)) {
            return false;
        }
        Set<Pair<Integer, Integer>> copy = GTUtil.copyOf(seen);
        copy.add(p);
        return this.left.isUniqueInstan(copy) && this.right.isUniqueInstan(copy);
    }

    @Override
    public boolean isRuntimeAware(GTSModelFactory mf, Theta theta) {
        if (this.committedLeft.isEmpty() && this.committedRight.isEmpty()) {
            return this.left.getRoles().equals(this.right.getRoles());  // awareness (2)
        }

        Set<Role> rs = getRoles();
        rs.removeAll(getIndifferent());

        Set<SAction<DynamicActionKind>> as = this.right.getWeakActsTop(mf, theta);  // !!! CHECKME R-acting def?  CHECKME weak OK?
        Set<Role> actingR = as.stream().map(x -> x.subj).collect(Collectors.toSet());
        actingR.removeAll(this.committedLeft);  // !!! CHECKME R-acting def?

        for (Role r : rs) {
            if (actingR.contains(r)
                    && !this.committedRight.contains(this.observer)) {  // awareness (1)  // !!! CHECKME this.observer (p)?  or r?
                return false;
            }
        }
        return true;
    }

    @Override
    public boolean isCoherent() {
        return (this.committedLeft.isEmpty() || this.committedRight.isEmpty())
                && this.left.isCoherent() && this.right.isCoherent();
    }

    /* ... */

    // Pre: this.committedLeft.contains(r) xor this.committedRight.contains(r)
    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        // Same as MixedChoice except with n -- XXX GTLCommitted
        if (this.committedLeft.contains(r) && !this.committedRight.contains(r)) {
            Optional<Pair<? extends GTLType, Sigma>> proj = this.left.project(rs, r, this.c, this.n);
            return proj.map(x -> Pair.of(lf.mixedCommitted(this.c, this.n, x.left, Side.LEFT), x.right));
        } else if (this.committedRight.contains(r) && !this.committedLeft.contains(r)) {
            Optional<Pair<? extends GTLType, Sigma>> proj = this.right.project(rs, r, this.c, this.n);
            return proj.map(x -> Pair.of(lf.mixedCommitted(this.c, this.n, x.left, Side.RIGHT), x.right));
        } else { //if (!this.committedLeft.contains(r) && !this.committedRight.contains(r)) {
            //throw new RuntimeException("TODO: ");  // p,q ??
            Optional<Pair<? extends GTLType, Sigma>> opt_l = this.left.project(rs, r, this.c, this.n);
            Optional<Pair<? extends GTLType, Sigma>> opt_r = this.right.project(rs, r, this.c, this.n);

            if (!r.equals(this.other) && !r.equals(this.observer)) {

                // HERE HERE FIXME need to distinguish I/O cases (merge vs. MC)

                System.out.println("bbbbbbb: " + this.left + " ,, " + this.right);
                System.out.println("bbbbbbb: " + opt_l + " ,, " + opt_r);

                if (!opt_l.isPresent() || !opt_r.isPresent()) {  // TODO refactor with below
                    return Optional.empty();
                }
                if (GTGMixedChoice.isMergableIOModes(opt_l.get().left, opt_r.get().left)) {
                    Optional<Pair<? extends GTLType, Sigma>> merged =
                            GTGMixedActive.mergePair(opt_l, opt_r);
                    if (!merged.isPresent()) { //optl.isEmpty() || optr.isEmpty()) {
                        return Optional.empty();
                    }

                } else {

                    // TODO FIXME MC conditions?
                    System.err.println("[Warning] TODO non-mergable MC projection conditions: "
                            + opt_l + " ,, " + opt_r);

                }
            }
            Pair<? extends GTLType, Sigma> get_l = opt_l.get();
            Pair<? extends GTLType, Sigma> get_r = opt_r.get();

            return Optional.of(Pair.of(
                    lf.mixedActive(this.c, this.n, get_l.left, get_r.left),
                    get_l.right.circ(get_r.right)));
            // TODO conditions? -- abstract global props should be implemented here?

            /*Sigma s0 = get_l.right.circ(get_r.right);
            if (r.equals(this.other) || r.equals(this.observer)) {
                return Optional.of(Pair.of(lf.mixedActive(this.c, this.n, get_l.left, get_r.left), s0));
            } else {

                if (GTGMixedChoice.isMergableIOModes(get_l.left, get_r.left)) {
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
                    if (!GTGMixedChoice.getPeer(get_l.left).equals(get_r.left)) {  // CHECKME currently no recursive check (cf. single-pointed, also merge)
                        return Optional.empty();
                    }

                    return Optional.of(Pair.of(lf.mixedActive(this.c, this.n, get_l.left, get_r.left), s0));
                }
            }*/
        }
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        Optional<Theta> thetaL = this.left.projectTheta(cs, r);
        if (!thetaL.isPresent()) {
            return thetaL;
        }
        Optional<Theta> thetaR = this.right.projectTheta(cs, r);
        if (!thetaR.isPresent()) {
            return thetaR;
        }
        Theta left = thetaL.get();
        Theta right = thetaR.get();
        return max(left, right).map(x -> {
            if (this.n + 1 > x.map.get(this.c)) {  // !!! n+1
                HashMap<Integer, Integer> map = new HashMap<>(x.map);
                map.put(this.c, this.n + 1);
                return new Theta(map);
            }
            return x;
        });
    }

    public static Optional<Theta> max(Theta t1, Theta t2) {
        if (!t1.map.keySet().equals(t2.map.keySet())) {
            Optional.empty();
        }
        Map<Integer, Integer> map = new HashMap<>(t1.map);
        for (Map.Entry<Integer, Integer> e : t2.map.entrySet()) {
            int k = e.getKey();
            int v = e.getValue();
            if (v > map.get(k)) {
                map.put(k, v);
            }
        }
        return Optional.of(new Theta(map));
    }

    /* ... */

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {

        LinkedHashSet<Role> cl = new LinkedHashSet<>(this.committedLeft);
        LinkedHashSet<Role> cr = new LinkedHashSet<>(this.committedRight);
        Either<Exception, Triple<Theta, GTGType, Tree<String>>> optl =
                this.committedRight.contains(a.subj)  // !!! [RTAct] needs more restrictions?
                        ? Either.left(newStuck(c, n, theta, this, (GTSAction) a))
                        : this.left.step(theta, a, this.c, this.n);
        Either<Exception, Triple<Theta, GTGType, Tree<String>>> optr =
                this.committedLeft.contains(a.subj)
                        ? Either.left(newStuck(c, n, theta, this, (GTSAction) a))
                        : this.right.step(theta, a, this.c, this.n);

        if (optl.isRight() && optr.isRight()) {
            // [RTAct]
            return Either.right(Triple.of(
                    theta,
                    this.fact.activeMixedChoice(this.c, this.n,
                            optl.getRight().mid,
                            optr.getRight().mid,
                            this.other, this.observer, cl, cr),
                    Tree.of("[RTAct][..discard..]")));  // TODO both opt strings (currently discarded)

        } else if (optl.isRight()) {
            if (optr.isRight() || this.committedRight.contains(a.subj)) {
                return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
            }
            Triple<Theta, GTGType, Tree<String>> get = optl.getRight();
            if (a.isReceive()) {
                String tag;
                if (this.committedLeft.contains(a.obj) || a.subj.equals(this.observer)) {  // XXX this.p => q ?
                    // [LRcv1]
                    cl.add(a.subj);  // !!! l* problem -- but why not always commit as in [lcrv] ?  [rrcv] will "correct" -- invariant: in l xor r, not both
                    tag = "[LRcv1]";
                } else {
                    // [LRcv2]
                    tag = "[LRcv2]";
                }
                GTGMixedActive succ = this.fact.activeMixedChoice(
                        this.c, this.n, get.mid, this.right, this.other, this.observer, cl, cr);
                Tree<String> rule = Tree.of(
                        toStepJudgeString(tag, c, n, theta, this, (GTSAction) a, get.left, succ),
                        get.right);
                return Either.right(Triple.of(get.left, succ, rule));
            } else if (a.isSend()) {  // [LSnd]
                GTGMixedActive succ = this.fact.activeMixedChoice(
                        this.c, this.n, get.mid, this.right, this.other, this.observer, cl, cr);
                return Either.right(Triple.of(get.left, succ, Tree.of(
                        toStepJudgeString("[LSnd]", c, n, theta, this, (GTSAction) a, get.left, succ),
                        get.right)));
            } else if (a instanceof GTSNewTimeout) {  // !!!
                GTGMixedActive succ = this.fact.activeMixedChoice(
                        this.c, this.n, get.mid, this.right, this.other, this.observer, cl, cr);
                return Either.right(Triple.of(get.left, succ, Tree.of(
                        toStepJudgeString("[..Ctx1..]", c, n, theta, this, (GTSAction) a, get.left, succ),
                        get.right)));
            } else {
                throw new RuntimeException("TODO: " + a);
            }

        } else if (optr.isRight()) {
            Triple<Theta, GTGType, Tree<String>> get = optr.getRight();  // May be empty for nested mixed choices in the "stuck" side
            if (a.isSend()) {
                // [RSnd]
                if (optl.isRight() || this.committedLeft.contains(a.subj)) {
                    return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
                }
                cr.add(a.subj);
                GTGMixedActive succ = this.fact.activeMixedChoice(
                        this.c, this.n, this.left, get.mid, this.other, this.observer, cl, cr);
                return Either.right(Triple.of(get.left, succ, Tree.of(
                        toStepJudgeString("[RSnd]", c, n, theta, this, (GTSAction) a, get.left, succ),
                        get.right)));

            } else if (a.isReceive()) {
                // [RRcv]
                if (optl.isRight()) {  // Redundant due to earlier
                    return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
                }
                //cl.remove(a.subj);  // old -- "committed" is now monotonic (committed for certain)
                cr.add(a.subj);
                GTGMixedActive succ = this.fact.activeMixedChoice(
                        this.c, this.n, this.left, get.mid, this.other, this.observer, cl, cr);
                return Either.right(Triple.of(get.left, succ, Tree.of(
                        toStepJudgeString("[RRcv]", c, n, theta, this, (GTSAction) a, get.left, succ),
                        get.right)));

            } else if (a instanceof GTSNewTimeout) {  // HACK
                GTGMixedActive succ = this.fact.activeMixedChoice(
                        this.c, this.n, this.left, get.mid, this.other, this.observer, cl, cr);
                return Either.right(Triple.of(get.left, succ, Tree.of(
                        toStepJudgeString("[..TO-HACK-R..]", c, n, theta, this, (GTSAction) a, get.left, succ),
                        get.right)));

            } else {
                throw new RuntimeException("TODO: " + a);
            }
        } else {
            return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
        }
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c,
            int n) {  // XXX outer still OK to reduce if inner is fully ended?

        Set<Role> bLeft = Stream.concat(blocked.stream(),
                this.committedRight.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction<DynamicActionKind>> aLeft = this.left.getActs(mf, theta, bLeft, this.c, this.n);
        Set<Role> bRight = Stream.concat(blocked.stream(),
                this.committedLeft.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction<DynamicActionKind>> aRight = this.right.getActs(mf, theta, bRight, this.c, this.n);
        aLeft.addAll(aRight);
        return aLeft;
    }

    /* ... */

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStep
            (
                    Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        return step(theta, a, c, n);
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getWeakActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c,
            int n) {
        return getActs(mf, theta, blocked, c, n);
    }

    /* ... */

    @Override
    public Set<Op> getCommittingTop(Set<Role> com) {
        throw new RuntimeException("Unsupported operation: " + this);
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        throw new RuntimeException("Unsupported operation: " + this);
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        throw new RuntimeException("Unsupported operation: " + this);
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    /* Aux */

    @Override
    public GTGMixedActive subs(Map<RecVar, GTGType> subs) {
        GTGType left = this.left.subs(subs);
        GTGType right = this.left.subs(subs);
        return new GTGMixedActive(this.c, this.n, left, right, this.other, this.observer,
                GTUtil.copyOf(this.committedLeft), GTUtil.copyOf(this.committedRight));  // FIXME repeated copying
    }

    @Override
    public GTGMixedActive unfoldAllOnce() {
        return this;
    }

    @Override
    public Set<Role> getRoles() {

        // !!! key design point: not including committed sets in of themselves -- cf. all properties predicated on roles(G), e.g., aware

        return GTUtil.union(
                GTUtil.minus(this.left.getRoles(), this.committedRight),
                GTUtil.minus(this.right.getRoles(), this.committedLeft));
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        Set<Integer> res = new HashSet<>();
        // !!! not adding this.c -- currently detect only inactive mixed
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
        return "(" + this.left + " " + this.committedLeft + " " + ConsoleColors.BLACK_TRIANGLE
                + this.c + "," + this.n
                + ":" + this.other + "->" + this.observer
                + " " + this.committedRight + " " + this.right + ")";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.MIXED_CHOICE_ACTIVE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.n;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        hash = 31 * hash + this.other.hashCode();
        hash = 31 * hash + this.observer.hashCode();
        hash = 31 * hash + this.committedLeft.hashCode();
        hash = 31 * hash + this.committedRight.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGMixedActive)) {
            return false;
        }
        GTGMixedActive them = (GTGMixedActive) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.n == them.n
                && this.left.equals(them.left)
                && this.right.equals(them.right)
                && this.other.equals(them.other)
                && this.observer.equals(them.observer)
                && this.committedLeft.equals(them.committedLeft)
                && this.committedRight.equals(them.committedRight);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGMixedActive;
    }
}
