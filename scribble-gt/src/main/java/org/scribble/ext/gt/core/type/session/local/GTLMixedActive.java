package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSNewTimeout;
import org.scribble.ext.gt.core.type.session.global.GTGInteraction;
import org.scribble.ext.gt.core.type.session.global.GTGType;
import org.scribble.ext.gt.core.type.session.global.GTGTypeFactory;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

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
    public GTLMixedActive unfoldContext(Map<RecVar, GTLType> env) {
        GTLType left = this.left.unfoldContext(env);
        GTLType right = this.left.unfoldContext(env);
        return this.fact.mixedActive(this.c, this.n, left, right);
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
        Optional<? extends GTLType> opt_l = this.left.merge(cast.left);
        Optional<? extends GTLType> opt_r = this.right.merge(cast.right);
        return opt_l.flatMap(x -> opt_r.map(y ->
                this.fact.mixedActive(this.c, this.n, x, y)));
    }

    // Pre: a in getActs
    // Deterministic w.r.t. a -- CHECKME: recursion
    // !!! TODO if all roles committed, can drop either l or r?
    @Override
    public Optional<GTLType> step(EAction a) {
        /*LinkedHashSet<Role> cl = new LinkedHashSet<>(this.committedLeft);
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
        }*/
        throw new RuntimeException("TODO");
    }

    @Override
    public LinkedHashSet<EAction> getActs(
            EModelFactory mf, Set<Role> blocked) {  // XXX outer still OK to reduce if inner is fully ended?
        /*Set<Role> bLeft = Stream.concat(blocked.stream(),
                this.committedRight.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aLeft = this.left.getActs(mf, theta, bLeft);
        Set<Role> bRight = Stream.concat(blocked.stream(),
                this.committedLeft.stream()).collect(Collectors.toSet());
        LinkedHashSet<SAction> aRight = this.right.getActs(mf, theta, bRight);
        aLeft.addAll(aRight);
        return aLeft;*/
        throw new RuntimeException("TODO");
    }

    /* Aux */

    @Override
    public String toString() {
        return "(" + this.left + " |>"
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
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLMixedActive)) return false;
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
