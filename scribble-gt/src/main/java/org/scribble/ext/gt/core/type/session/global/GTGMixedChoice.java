package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.local.GTLType;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// HERE extend ANTLR -- copy frontend stuff from scrib-assrt
public class GTGMixedChoice implements GTGType {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final GTGType left;
    public final GTGType right;
    public final Role o;  // TODO: rename  -- observer?  "original monitor?"
    public final Set<Role> committedLeft;
    public final Set<Role> committedRight;

    protected GTGMixedChoice(
            GTGType left, GTGType right, Role o,
            LinkedHashSet<Role> committedLeft, LinkedHashSet<Role> committedRight) {
        this.left = left;
        this.right = right;
        this.o = o;
        this.committedLeft = Collections.unmodifiableSet(
                new LinkedHashSet<>(committedLeft));
        this.committedRight = Collections.unmodifiableSet(
                new LinkedHashSet<>(committedRight));
    }

    @Override
    public Optional<? extends GTLType> project(Role r) {
        if (!this.committedRight.contains(r) &&
                (!this.committedLeft.contains(r) || this.o.equals(r))) {
            return this.left.project(r);
        } else if (!this.committedLeft.contains(r) && this.committedRight.contains(r)) {
            return this.right.project(r);
        } else if (!this.committedLeft.contains(r) && !this.committedRight.contains(r)) {  // !!! XXX overlaps with above
            throw new RuntimeException("TODO: ");  // p,q ??
        } else {
            return Optional.empty();  // !!! CHECKME: or error
        }
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
        return left.src.equals(right.dst) && left.dst.equals(right.src)
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
        Optional<GTGType> opt = this.left.step(a);
        if (opt.isPresent()) {
            GTGType get = opt.get();
            if (a.isReceive()) {
                cl.add(a.subj);  // !!! l* problem -- but why not always commit as in [lcrv] ?  [rrcv] will "correct" -- invariant: in l xor r, not both
            } else if (!a.isSend()) {
                throw new RuntimeException("TODO: " + a);
            }
            return Optional.of(this.fact.mixedChoice(get, this.right, this.o, cl, cr));
        } else {
            GTGType get = this.right.step(a).get();  // Pre: a in getActs, so non-empty
            if (a.isSend()) {
                cr.add(a.subj);
            } else if (a.isReceive()) {
                cl.remove(a.subj);
                cr.add(a.subj);
            } else {
                throw new RuntimeException("TODO: " + a);
            }
            return Optional.of(this.fact.mixedChoice(this.left, get, this.o, cl, cr));
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
        return this.left + " " + this.committedLeft + " |>" + this.o + " "
                + this.committedRight + " " + this.right;
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.MIXED_CHOICE_HASH;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        hash = 31 * hash + this.o.hashCode();
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
                && this.o.equals(them.o)
                && this.committedLeft.equals(them.committedLeft)
                && this.committedRight.equals(them.committedRight);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGMixedChoice;
    }
}
