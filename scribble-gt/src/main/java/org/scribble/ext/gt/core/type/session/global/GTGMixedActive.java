package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
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

    @Override
    public GTGMixedActive unfoldContext(Map<RecVar, GTGType> c) {
        GTGType left = this.left.unfoldContext(c);
        GTGType right = this.left.unfoldContext(c);
        return new GTGMixedActive(this.c, this.n, left, right, this.other, this.observer,
                new LinkedHashSet<>(this.committedLeft), new LinkedHashSet<>(this.committedRight));  // FIXME repeated copying
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
    public Optional<? extends GTLType> project(Role r) {
        // Same as MixedChoice except with n
        if (!this.committedRight.contains(r) &&
                (this.committedLeft.contains(r) || this.observer.equals(r))) {  // XXX p.equals(r) ???
            return this.left.project(r);
        } else if (!this.committedLeft.contains(r) && this.committedRight.contains(r)) {
            return this.right.project(r);
        } else if (!this.committedLeft.contains(r) && !this.committedRight.contains(r)) {
            //throw new RuntimeException("TODO: ");  // p,q ??
            GTLTypeFactory lf = GTLTypeFactory.FACTORY;
            Optional<? extends GTLType> optl = this.left.project(r);
            Optional<? extends GTLType> optr = this.right.project(r);
            if (optl.isEmpty() || optr.isEmpty()) {
                return Optional.empty();
            }
            GTLType getl = optl.get();
            GTLType getr = optr.get();
            if (!r.equals(this.observer) && !r.equals(this.other)) {
                if (!getl.equals(getr)) {  // !!! TODO merge -- !!! maybe don't need merge here to start
                    return Optional.empty();
                }
            }
            return Optional.of(lf.mixedActive(this.c, this.n, getl, getr));
        }
        return Optional.empty();  // !!! CHECKME: or error
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

    @Override
    public boolean isCoherent() {
        return (this.committedLeft.isEmpty() || this.committedRight.isEmpty())
                && this.left.isCoherent() && this.right.isCoherent();
    }

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<Pair<Theta, GTGType>> step(Theta theta, SAction a) {
        LinkedHashSet<Role> cl = new LinkedHashSet<>(this.committedLeft);
        LinkedHashSet<Role> cr = new LinkedHashSet<>(this.committedRight);
        Optional<Pair<Theta, GTGType>> optl = this.left.step(theta, a);
        Optional<Pair<Theta, GTGType>> optr = this.right.step(theta, a);
        if (optl.isPresent() && optr.isPresent()) {
            // [RTAct]
            // !!! CHECKME: check something re. this.p/q and a ?
            return Optional.of(new Pair<>(
                    theta,
                    this.fact.activeMixedChoice(this.c, this.n,
                            optl.get().right,
                            optr.get().right,
                            this.other, this.observer, cl, cr)));
        } else if (optl.isPresent()) {
            if (optr.isPresent() || this.committedRight.contains(a.subj)) {  // First cond is redundant
                return Optional.empty();
            }
            Pair<Theta, GTGType> get = optl.get();
            if (a.isReceive()) {
                if (this.committedLeft.contains(a.obj) || a.subj.equals(this.observer)) {  // XXX this.p => q ?
                    // [LRcv1]
                    cl.add(a.subj);  // !!! l* problem -- but why not always commit as in [lcrv] ?  [rrcv] will "correct" -- invariant: in l xor r, not both
                } else {
                    // [LRcv2]
                }
                return Optional.of(new Pair<>(
                        theta,
                        this.fact.activeMixedChoice(this.c, this.n, get.right, this.right, this.other, this.observer, cl, cr)));
            } else if (a.isSend()) {  // [LSnd]
                return Optional.of(new Pair<>(
                        theta,
                        this.fact.activeMixedChoice(this.c, this.n, get.right, this.right, this.other, this.observer, cl, cr)));

            } else if (a instanceof GTSNewTimeout) {  // Hack
                return Optional.of(new Pair<>(
                        get.left,
                        this.fact.activeMixedChoice(this.c, this.n, get.right, this.right, this.other, this.observer, cl, cr)));

            } else {
                throw new RuntimeException("TODO: " + a);
            }
        } else if (optr.isPresent()) {
            Pair<Theta, GTGType> get = optr.get();  // May be empty for nested mixed choices in the "stuck" side
            if (a.isSend()) {
                // [RSnd]
                if (optl.isPresent() || this.committedLeft.contains(a.subj)) {  // First cond is redundant
                    return Optional.empty();
                }
                cr.add(a.subj);
                return Optional.of(new Pair<>(
                        theta,
                        this.fact.activeMixedChoice(this.c, this.n, this.left, get.right, this.other, this.observer, cl, cr)));
            } else if (a.isReceive()) {
                // [RRcv]
                if (optl.isPresent()) {  // Redundant in this code
                    return Optional.empty();
                }
                //cl.remove(a.subj);  // old -- "committed" is now monotonic (committed for certain)
                cr.add(a.subj);
                return Optional.of(new Pair<>(
                        theta,
                        this.fact.activeMixedChoice(this.c, this.n, this.left, get.right, this.other, this.observer, cl, cr)));

            } else if (a instanceof GTSNewTimeout) {  // Hack
                return Optional.of(new Pair<>(
                        get.left,
                        this.fact.activeMixedChoice(this.c, this.n, this.left, get.right, this.other, this.observer, cl, cr)));

            } else {
                throw new RuntimeException("TODO: " + a);
            }
        } else {
            return Optional.empty();
        }
    }

    @Override
    public LinkedHashSet<SAction> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked) {  // XXX outer still OK to reduce if inner is fully ended?
        Set<Role> bLeft = Stream.concat(blocked.stream(),
                this.committedRight.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aLeft = this.left.getActs(mf, theta, bLeft);
        Set<Role> bRight = Stream.concat(blocked.stream(),
                this.committedLeft.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aRight = this.right.getActs(mf, theta, bRight);
        aLeft.addAll(aRight);
        return aLeft;
    }

    @Override
    public Set<Op> getOps() {
        Set<Op> ops = new HashSet<>(this.left.getOps());
        ops.addAll(this.right.getOps());
        return ops;
    }

    /* Aux */

    @Override
    public String toString() {
        return "(" + this.left + " " + this.committedLeft + " |>"
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
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGMixedActive)) return false;
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
