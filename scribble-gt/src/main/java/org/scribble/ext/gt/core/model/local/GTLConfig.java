package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.Quad;
import org.scribble.ext.gt.util.Tree;
import org.scribble.util.Pair;

import java.util.*;

public class GTLConfig {

    public final Role self;
    public final GTLType type;
    public final Sigma sigma;
    public final Theta theta;

    public GTLConfig(Role self, GTLType type, Sigma sigma, Theta theta) {
        this.self = self;
        this.type = type;
        this.sigma = sigma;
        this.theta = theta;
    }

    public LinkedHashSet<EAction<DynamicActionKind>> getActs(GTEModelFactory mf) {
        return this.type.getActsTop(mf, this.self, this.sigma, this.theta);
    }

    // n.b., GTESend only updates this local sender config -- use enqueueMessage to also update the receiver config
    public Either<Exception, Pair<GTLConfig, Tree<String>>> step(
            Set<Op> com, EAction<DynamicActionKind> a) {
        if (!(a instanceof GTEAction)) {
            throw new RuntimeException("TODO: " + a);  // cf. weak
        }
        Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> opt =
                this.type.stepTop(com, this.self, a, this.sigma, this.theta);
        return opt.mapRight(x -> Pair.of(
                new GTLConfig(this.self, x.fst, x.snd, x.thrd),
                x.frth));
    }

    public Pair<GTLConfig, Tree<String>> gc() {
        Map<Integer, Integer> active = this.type.getActive(this.theta);
        Sigma res = this.sigma.gc(active);
        return Pair.of(
                new GTLConfig(this.self, this.type, res, this.theta),
                Tree.of("[..GC..]  " + this.theta + ", " + this.type + " "
                        + ConsoleColors.VDASH + " " + this.sigma + " "
                        + ConsoleColors.RIGHT_ARROW + " " + res)  // cf. GTLType.toStepJudgeString
        );
    }

    // Does --tau(nu)-->* --a-->    -- cf. GTLSystem also does --tau(nu)-->* after a  (and gc)
    // TODO factor out with above
    public Either<Exception, Pair<GTLConfig, Tree<String>>> weakStep(
            Set<Op> com, EAction<DynamicActionKind> a) {
        if (!(a instanceof GTEAction)) {
            throw new RuntimeException("Shouldn't get in here: " + a);  // !!! weak
        }
        Either<Exception, Quad<GTLType, Sigma, Theta, Tree<String>>> opt =
                this.type.weakStepTop(com, this.self, a, this.sigma, this.theta);
        return opt.mapRight(x -> Pair.of(
                new GTLConfig(this.self, x.fst, x.snd, x.thrd),
                x.frth));
    }

    public GTLConfig enqueueMessage(Role src, GTESend<DynamicActionKind> a) {
        if (!this.self.equals(a.peer)) {
            throw new RuntimeException("Shouldn't get in here: " + a);
        }
        Map<Role, List<GTESend<DynamicActionKind>>> map = new HashMap<>(this.sigma.map);
        List<GTESend<DynamicActionKind>> ms = new LinkedList<>(map.get(src));
        ms.add(a);
        map.put(src, ms);
        Sigma sigma = new Sigma(map);
        return new GTLConfig(this.self, this.type, sigma, this.theta);
    }

    // !!! -- cf. equals
    public boolean isSubtype(GTLConfig sup) {
        return this.self.equals(sup.self) && GTLConfig.isSubtype(this.type, sup.type)
                && this.sigma.equals(sup.sigma) && this.theta.equals(sup.theta);
    }

    // Works in this framework because starting from common global rec -- no need to compare completely arbitrary (un)foldings
    static Map<GTLRecursion, List<GTLType>> unfoldings = new HashMap<>();  // strict (not reflexive)
    //static Map<GTLType, Set<GTLType>> subtypes = new HashMap<>();  // Could use in conjunction with unfoldings...

    // CHECKME: algorithmic MPST subtyping?
    // terminates assuming contractive -- recs will eventually unfold into non-recs
    public static boolean isSubtype(GTLType sub, GTLType sup) {
        if (sup.equals(sub)) {  // GTLEnd, GTLRecVar
            return true;
        } else if (sup instanceof GTLRecursion) {
            //return t.unfold().equals(u);
            //return isUnfolding((GTLRecursion) sup, sub);

            GTLRecursion cast = (GTLRecursion) sup;
            List<GTLType> tmp = unfoldings.computeIfAbsent(cast, k -> new LinkedList<>());
            if (tmp.stream().anyMatch(x -> isSubtype(sub, x))) {
                return true;
            }
            GTLType next;
            if (tmp.isEmpty()) {
                next = cast.unfoldAllOnce();
            } else {
                next = tmp.get(tmp.size() - 1).unfoldAllOnce();
            }
            tmp.add(next);
            return isSubtype(sub, next);

        } else if (sub instanceof GTLRecursion) {
            //return isUnfolding((GTLRecursion) sub, sup);

            GTLRecursion cast = (GTLRecursion) sub;
            List<GTLType> tmp = unfoldings.computeIfAbsent(cast, k -> new LinkedList<>());
            if (tmp.stream().anyMatch(x -> isSubtype(x, sup))) {
                return true;
            }
            GTLType next;
            if (tmp.isEmpty()) {
                next = cast.unfoldAllOnce();
            } else {
                next = tmp.get(tmp.size() - 1).unfoldAllOnce();
            }
            tmp.add(next);
            return isSubtype(next, sup);
        }

        if (sup instanceof GTLBranch) {
            if (!(sub instanceof GTLBranch)) {
                return false;
            }
            GTLBranch sup1 = (GTLBranch) sup;
            GTLBranch sub1 = (GTLBranch) sub;
            if (!sub1.cases.keySet().containsAll(sup1.cases.keySet())) {
                return false;
            }
            return sup1.cases.keySet().stream()
                    .allMatch(x -> isSubtype(sub1.cases.get(x), sup1.cases.get(x)));
        } else if (sup instanceof GTLSelect) {
            if (!(sub instanceof GTLSelect)) {
                return false;
            }
            GTLSelect sup1 = (GTLSelect) sup;
            GTLSelect sub1 = (GTLSelect) sub;
            if (!sup1.cases.keySet().containsAll(sub1.cases.keySet())) {
                return false;
            }
            return sub1.cases.keySet().stream()
                    .allMatch(x -> isSubtype(sub1.cases.get(x), sup1.cases.get(x)));
        }/* else if (sup instanceof GTLMixedChoice) {
        }*/ else if (sup instanceof GTLMixedActive) {
            throw new RuntimeException("TODO: " + sub + " <: " + sup);
        } else {
            throw new RuntimeException("TODO: " + sub + " <: " + sup);
        }
    }

    @Override
    public String toString() {
        return "<" + this.self + ", " + this.type + ", " + this.sigma + ", " +
                this.theta + ">";
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 49121;
        hash = 31 * hash + this.self.hashCode();
        hash = 31 * hash + this.type.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        hash = 31 * hash + this.theta.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLConfig)) return false;
        GTLConfig them = (GTLConfig) obj;
        return this.self.equals(them.self)
                && this.type.equals(them.type)
                && this.sigma.equals(them.sigma)
                && this.theta.equals(them.theta);
    }
}
