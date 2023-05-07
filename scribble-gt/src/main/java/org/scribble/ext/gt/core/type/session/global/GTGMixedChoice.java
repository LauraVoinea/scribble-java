package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.ext.gt.util.Triple;
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

    /* ... */

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
        return !r.equals(this.other) && !r.equals(this.observer)
                ? get_l.left.merge(get_r.left).map(x -> new Pair<>(x, s0))  // !!! refactor with GTGInteraction.merge
                : Optional.of(new Pair<>(lf.mixedChoice(this.c, get_l.left, get_r.left), s0));
    }

    /* ... */

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<Triple<Theta, GTGType, String>> step(Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        if (!(a instanceof GTSNewTimeout)) {  // E.g., (rec) context rule may "attempt"
            return Optional.empty();
        }
        GTSNewTimeout<?> nu = (GTSNewTimeout<?>) a;
        /*Map<Integer, Integer> tmp = new HashMap<>(theta.map);
        tmp.put(nu.c, tmp.get(nu.c) + 1);
        Theta theta1 = new Theta(tmp);*/
        Theta theta1 = theta.inc(this.c);

        // FIXME use factory?
        GTGMixedActive active = new GTGMixedActive(nu.c, nu.n,
                this.left, this.right, this.other, this.observer,
                new LinkedHashSet<>(), new LinkedHashSet<>());

        return Optional.of(new Triple<>(theta1, active, "[Inst]"));
    }

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

    /* Aux */

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
