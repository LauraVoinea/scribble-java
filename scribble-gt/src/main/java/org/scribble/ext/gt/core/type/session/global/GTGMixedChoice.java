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

    @Override
    public GTGMixedChoice unfoldContext(Map<RecVar, GTGType> c) {
        GTGType left = this.left.unfoldContext(c);
        GTGType right = this.right.unfoldContext(c);
        return new GTGMixedChoice(this.c, left, right, this.other, this.observer);
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
    public Optional<? extends GTLType> project(Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        Optional<? extends GTLType> optl = this.left.project(r);
        Optional<? extends GTLType> optr = this.right.project(r);
        if (optl.isEmpty() || optr.isEmpty()) {
            return Optional.empty();
        }
        GTLType getl = optl.get();
        GTLType getr = optr.get();
        return !r.equals(this.observer) && !r.equals(this.other)
                ? getl.merge(getr)
                : Optional.of(lf.mixedChoice(getl, getr));
    }

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

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<Pair<Theta, GTGType>> step(Theta theta, SAction a) {
        Map<Integer, Integer> tmp = new HashMap<>(theta.map);
        GTSNewTimeout nu = (GTSNewTimeout) a;
        tmp.put(nu.c, tmp.get(nu.c) + 1);
        Theta theta1 = new Theta(tmp);

        // FIXME use factory?
        GTGMixedActive active = new GTGMixedActive(nu.c, nu.n,
                this.left, this.right, this.other, this.observer,
                new LinkedHashSet<>(), new LinkedHashSet<>());

        return Optional.of(new Pair<>(theta1, active));
    }

    @Override
    public LinkedHashSet<SAction> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked) {
        LinkedHashSet<SAction> res = new LinkedHashSet<>();
        if (theta.map.containsKey(this.c)) {
            res.add(mf.SNewTimeout(this.c, theta.map.get(this.c)));
        }
        return res;
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
        return "(" + this.left + " |>"
                + this.c + ":" + this.other + "->" + this.observer
                + " " + this.right + ")";
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
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGMixedChoice)) return false;
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
