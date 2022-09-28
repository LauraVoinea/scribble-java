package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// HERE extend ANTLR -- copy frontend stuff from scrib-assrt
public class GTGMixedChoice implements GTGType {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final GTGType left;
    public final GTGType right;
    public final Role p;  // TODO: rename  -- observer?  "original monitor?"
    public final Role q;  // q->p ... |> p->q
    public final Set<Role> committedLeft;
    public final Set<Role> committedRight;

    protected GTGMixedChoice(
            GTGType left, GTGType right, Role o, Role t,
            LinkedHashSet<Role> committedLeft, LinkedHashSet<Role> committedRight) {
        this.left = left;
        this.right = right;
        this.p = o;
        this.q = t;
        this.committedLeft = Collections.unmodifiableSet(
                new LinkedHashSet<>(committedLeft));
        this.committedRight = Collections.unmodifiableSet(
                new LinkedHashSet<>(committedRight));
    }

    @Override
    public Optional<? extends GTLType> project(Role r) {
        if (!this.committedRight.contains(r) &&
                (this.committedLeft.contains(r) || this.p.equals(r))) {  // XXX p.equals(r) ???
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
            if (!r.equals(this.p) && !r.equals(this.q)) {
                if (!getl.equals(getr)) {  // !!! TODO merge -- !!! maybe don't need merge here to start
                    return Optional.empty();
                }
            }
            return Optional.of(lf.mixedChoice(getl, getr));
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
        return left.src.equals(right.dst) && right.dst.equals(this.q)
                && left.dst.equals(right.src) && right.src.equals(this.p)
                && this.left.isSinglePointed() && this.right.isSinglePointed();
    }

    @Override
    public boolean isGood() {
        return (this.committedLeft.isEmpty() || this.committedRight.isEmpty())
                && this.left.isGood() && this.right.isGood();
    }

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<GTGType> step(SAction a) {
        LinkedHashSet<Role> cl = new LinkedHashSet<>(this.committedLeft);
        LinkedHashSet<Role> cr = new LinkedHashSet<>(this.committedRight);
        Optional<GTGType> optl = this.left.step(a);
        Optional<GTGType> optr = this.right.step(a);
        if (optl.isPresent() && optr.isPresent()) {
            // [RTAct]
            // !!! CHECKME: check something re. this.p/q and a ?
            return Optional.of(
                    this.fact.mixedChoice(optl.get(), optr.get(), this.p, this.q, cl, cr));
        }
        else if (optl.isPresent()) {
            GTGType get = optl.get();
            if (a.isReceive()) {
                if (this.right.step(a).isPresent() || this.committedRight.contains(a.subj)) {
                    return Optional.empty();
                }
                if (this.committedLeft.contains(a.obj) || a.obj.equals(this.q)) {  // XXX this.p => q ?
                    // [LRcv1]
                    cl.add(a.subj);  // !!! l* problem -- but why not always commit as in [lcrv] ?  [rrcv] will "correct" -- invariant: in l xor r, not both
                } else {
                    // [LRcv2]
                }
                return Optional.of(
                        this.fact.mixedChoice(get, this.right, this.p, this.q, cl, cr));
            } else if (a.isSend()) {  // [LSnd]
                if (this.committedRight.contains(a.subj) || this.right.step(a).isPresent()) {
                    return Optional.empty();
                }
                return Optional.of(
                        this.fact.mixedChoice(get, this.right, this.p, this.q, cl, cr));
            }
            throw new RuntimeException("TODO: " + a);
        } else {
            GTGType get = optr.get();  // Pre: a in getActs, so non-empty
            if (a.isSend()) {
                // [RSnd]
                if (this.left.step(a).isPresent()  // ???
                        || this.committedLeft.contains(a.subj)) {
                    return Optional.empty();
                }
                cr.add(a.subj);
                return Optional.of(
                        this.fact.mixedChoice(this.left, get, this.p, this.q, cl, cr));
            } else if (a.isReceive()) {
                // [RRcv]
                if (this.left.step(a).isPresent()) {
                    return Optional.empty();
                }
                //cl.remove(a.subj);  // old -- "committed" is now monotonic (committed for certain)
                cr.add(a.subj);
                return Optional.of(
                        this.fact.mixedChoice(this.left, get, this.p, this.q, cl, cr));
            }
            throw new RuntimeException("TODO: " + a);
        }
    }

    @Override
    public LinkedHashSet<SAction> getActs(SModelFactory mf, Set<Role> blocked) {
        Set<Role> bLeft = Stream.concat(blocked.stream(),
                this.committedRight.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aLeft = this.left.getActs(mf, bLeft);
        Set<Role> bRight = Stream.concat(blocked.stream(),
                this.committedLeft.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aRight = this.right.getActs(mf, bRight);
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
        return this.left + " " + this.committedLeft + " |>" + this.p + "->"
                + this.q + " " + this.committedRight + " " + this.right;
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.MIXED_CHOICE_HASH;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        hash = 31 * hash + this.p.hashCode();
        hash = 31 * hash + this.q.hashCode();
        hash = 31 * hash + this.committedLeft.hashCode();
        hash = 31 * hash + this.committedRight.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGMixedChoice)) return false;
        GTGMixedChoice them = (GTGMixedChoice) obj;
        return them.canEquals(this)
                && this.left.equals(them.left)
                && this.right.equals(them.right)
                && this.p.equals(them.p)
                && this.q.equals(them.q)
                && this.committedLeft.equals(them.committedLeft)
                && this.committedRight.equals(them.committedRight);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGMixedChoice;
    }
}
