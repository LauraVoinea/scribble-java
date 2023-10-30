package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.util.*;
import org.scribble.util.Pair;

import java.util.*;

public class GTLConfig {

    public final Role self;
    public final GTLType type;
    public final Sigma sigma;
    public final Theta theta;

    public final Map<Pair<Integer, Integer>, Discard> discard;  // key is c, n

    public GTLConfig(Role self, GTLType type, Sigma sigma, Theta theta,
                     Map<Pair<Integer, Integer>, Discard> discard) {
        this.self = self;
        this.type = type;
        this.sigma = sigma;
        this.theta = theta;
        this.discard = GTUtil.copyOf(discard);
    }

    /* ... */

    //public LinkedHashSet<EAction<DynamicActionKind>> getActs(GTEModelFactory mf) {
    public LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> getActs(GTEModelFactory mf) {
        return this.type.getActsTop(mf, this.self, this.sigma, this.theta);
    }

    // !!! formal LTS is inductively defined LTS on configs -- cf. below, delegated to GTLType

    // n.b., GTESend only updates this local sender config -- use enqueueMessage to also update the receiver config
    public Either<Exception, Pair<GTLConfig, Tree<String>>> step(
            Set<Op> com, EAction<DynamicActionKind> a) {
        if (!(a instanceof GTEAction)) {
            throw new RuntimeException("TODO: " + a + " ,, " + a.getClass());  // cf. weak
        }
        Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>>> opt =
                this.type.stepTop(com, this.self, a, this.sigma, this.theta);

        return opt.mapRight(x -> {
            Map<Pair<Integer, Integer>, Discard> d1 = GTUtil.copyOf(this.discard);
            d1.putAll(x.right);
            return Pair.of(
                    new GTLConfig(this.self, x.left.fst, x.left.snd, x.left.thrd, d1),
                    x.left.frth);
        });
    }

    public Pair<GTLConfig, Tree<String>> gc(Map<Integer, Pair<Set<Op>, Set<Op>>> labs) {
        //Map<Integer, Integer> active = this.type.getActive(this.theta);

        //System.out.println("77777777: " + this.type + " ,, " + labs + " ,, " + this.discard);

        Sigma res = this.sigma.gc(labs, this.discard);
        return Pair.of(
                new GTLConfig(this.self, this.type, res, this.theta, this.discard),
                Tree.of("[..GC..]  " + this.theta + ", " + this.type + " "
                        + ConsoleColors.VDASH + " " + this.sigma + " "
                        + ConsoleColors.RIGHT_ARROW + " " + res)  // cf. GTLType.toStepJudgeString
        );
    }

    public Pair<GTLConfig, Tree<String>> gc() {

        Map<Integer, Integer> active = this.type.getActive(this.theta);

        /*  // FIXME EXPERIMENTAL HACK -- doesn't work, when last black committed STILL need to distinguish left vs. right messages (discard non-taken side)
        if (active.isEmpty()) {
            active = GTUtil.copyOf(this.theta.map).entrySet()
                    .stream().collect(Collectors.toMap(
                            Map.Entry::getKey,
                            x -> x.getValue() - 1
                    ));
        }
        //*/

        System.out.println("6666666: " + this.type + " ,, " + active);

        Sigma res = this.sigma.gc(active);  // XXX when GTLCommitted need to discard only left/right labs accordingly
        return Pair.of(
                new GTLConfig(this.self, this.type, res, this.theta, this.discard),
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
        Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>, Map<Pair<Integer, Integer>, Discard>>> opt =
                this.type.weakStepTop(com, this.self, a, this.sigma, this.theta);
        return opt.mapRight(x -> {
            Map<Pair<Integer, Integer>, Discard> d1 = GTUtil.copyOf(this.discard);
            d1.putAll(x.right);
            return Pair.of(
                    new GTLConfig(this.self, x.left.fst, x.left.snd, x.left.thrd, d1),
                    x.left.frth);
        });
    }

    public GTLConfig enqueueMessage(Role src, GTESend<DynamicActionKind> a) {
        if (!this.self.equals(a.peer) || !this.sigma.map.containsKey(src)) {
            throw new RuntimeException("Shouldn't get in here: " + a);  // cf. Either -- stuck
        }
        Map<Role, List<GTESend<DynamicActionKind>>> map = new HashMap<>(this.sigma.map);
        List<GTESend<DynamicActionKind>> ms = new LinkedList<>(map.get(src));
        ms.add(a);
        map.put(src, ms);
        Sigma sigma = new Sigma(map);
        return new GTLConfig(this.self, this.type, sigma, this.theta, this.discard);
    }

    // !!! -- cf. equals
    public boolean isSubtype(GTLConfig sup) {

        return this.self.equals(sup.self)
                //&& this.type.equals(sup.type)
                && GTLConfig.isSubtype(this.theta, this.type, sup.type)
                && this.sigma.equals(sup.sigma);

        // TODO FIXME
        //&& this.theta.equals(sup.theta);
        // ...also this.discard

    }

    // Works in this framework because starting from common global rec -- no need to compare completely arbitrary (un)foldings
    static Map<GTLRecursion, List<GTLType>> unfoldings = new HashMap<>();  // strict (not reflexive)
    //static Map<GTLType, Set<GTLType>> subtypes = new HashMap<>();  // Could use in conjunction with unfoldings...

    // CHECKME: algorithmic MPST subtyping?
    // terminates assuming contractive -- recs will eventually unfold into non-recs
    //public static boolean isSubtype(GTLType sub, GTLType sup) {
    public static boolean isSubtype(Theta theta, GTLType sub, GTLType sup) {

        if (sub.equals(sup)) {  // GTLEnd, GTLRecVar
            return true;
        }

        //*  // Needed for checkExcection1 (weak steps as pre step)
        else if (sup instanceof GTLRecursion) {
            //return t.unfold().equals(u);
            //return isUnfolding((GTLRecursion) sup, sub);

            GTLRecursion cast = (GTLRecursion) sup;
            List<GTLType> tmp = unfoldings.computeIfAbsent(cast, k -> new LinkedList<>());
            if (tmp.stream().anyMatch(x -> isSubtype(theta, sub, x))) {
                return true;
            }
            GTLType next;
            if (tmp.isEmpty()) {
                next = cast.unfoldAllOnce();
            } else {
                next = tmp.get(tmp.size() - 1).unfoldAllOnce();
            }
            tmp.add(next);
            return isSubtype(theta, sub, next);
        }
        //*/
        else if (sub instanceof GTLRecursion) {
            //return isUnfolding((GTLRecursion) sub, sup);

            GTLRecursion cast = (GTLRecursion) sub;
            List<GTLType> tmp = unfoldings.computeIfAbsent(cast, k -> new LinkedList<>());
            if (tmp.stream().anyMatch(x -> isSubtype(theta, x, sup))) {
                return true;
            }
            GTLType next;
            if (tmp.isEmpty()) {
                next = cast.unfoldAllOnce();
            } else {
                next = tmp.get(tmp.size() - 1).unfoldAllOnce();
            }
            tmp.add(next);
            return isSubtype(theta, next, sup);
        }

        if (sup instanceof GTLBranch) {  // TODO switch sub/sup order
            if (!(sub instanceof GTLBranch)) {
                return false;
            }
            GTLBranch sup1 = (GTLBranch) sup;
            GTLBranch sub1 = (GTLBranch) sub;
            if (!sub1.cases.keySet().containsAll(sup1.cases.keySet())) {
                return false;
            }
            return sup1.cases.keySet().stream()
                    .allMatch(x -> isSubtype(theta, sub1.cases.get(x), sup1.cases.get(x)));
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
                    .allMatch(x -> isSubtype(theta, sub1.cases.get(x), sup1.cases.get(x)));
        }

        //
        else if (sub instanceof GTLMixedChoice) {
            // !!! TODO structural case needed for white ?

            if (sup instanceof GTLMixedActive) {
                GTLMixedChoice sub_cast = (GTLMixedChoice) sub;
                GTLMixedActive sup_cast = (GTLMixedActive) sup;
                if (sub_cast.c == sup_cast.c && theta.map.get(sub_cast.c) <= sup_cast.n) {  // HERE HERE refactor theta -1 offset
                    return isSubtype(theta, sub_cast.left, sup_cast.left)
                            && isSubtype(theta, sub_cast.right, sup_cast.right);
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }

        //
        else if (sub instanceof GTLMixedActive) {
            if (!(sup instanceof GTLMixedActive)) {
                return false;
            }
            GTLMixedActive sub_cast = (GTLMixedActive) sub;
            GTLMixedActive sup_cast = (GTLMixedActive) sup;
            return isSubtype(theta, sub_cast.left, sup_cast.left)
                    && isSubtype(theta, sub_cast.right, sup_cast.right);
        }


        // TODO MixedCommitted

        else {
            throw new RuntimeException("TODO: " + sub + " <: " + sup);
        }
    }

    @Override
    public String toString() {
        return "<" + this.self + ", " + this.type + ", " + this.sigma + ", " +
                this.theta + ", " + this.discard + ">";
    }

    /* ... */

    @Override
    public int hashCode() {
        int hash = 49121;
        hash = 31 * hash + this.self.hashCode();
        hash = 31 * hash + this.type.hashCode();
        hash = 31 * hash + this.sigma.hashCode();
        hash = 31 * hash + this.theta.hashCode();
        hash = 31 * hash + this.discard.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLConfig)) { return false; }
        GTLConfig them = (GTLConfig) obj;
        return this.self.equals(them.self)
                && this.type.equals(them.type)
                && this.sigma.equals(them.sigma)
                && this.theta.equals(them.theta)
                && this.discard.equals(them.discard);
    }
}
