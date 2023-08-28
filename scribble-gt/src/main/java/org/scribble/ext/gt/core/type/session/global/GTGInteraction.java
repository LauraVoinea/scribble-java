package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.actions.SSend;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.global.action.GTSSend;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.ext.gt.util.Tree;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTGInteraction implements GTGType {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final Role src;
    public final Role dst;
    public final Map<Op, GTGType> cases;  // Pre: "Ordered", Unmodifiable, non-empty

    protected GTGInteraction(Role src, Role dst, LinkedHashMap<Op, GTGType> cases) {
        this.src = src;
        this.dst = dst;
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    /* ... */

    @Override
    public boolean isSinglePointed() {
        return this.cases.values().stream().allMatch(GTGType::isSinglePointed);
    }

    @Override
    public boolean isGood() {
        return this.cases.values().stream().allMatch(GTGType::isGood);
    }

    /* ... */

    @Override
    public boolean isInitial() {
        return this.cases.values().stream().allMatch(GTGType::isInitial);
    }

    // TODO refactor using choice-partic, and timeout-partic/pattern
    @Override
    public boolean isInitialWellSet(Set<Integer> cs) {
        if (this.cases.size() == 1) {
            return true;
        }
        GTGType fst = this.cases.values().iterator().next();
        if (!fst.isInitialWellSet(cs)) {
            return false;
        }

        // !!! cf. def 4
        Set<Role> rs = GTUtil.union(fst.getRoles(), Set.of(this.src, this.dst));
        return this.cases.values().stream().skip(1).allMatch(x ->

                // !!!
                GTUtil.union(x.getRoles(), Set.of(this.src, this.dst)).equals(rs)  // "static" choice participation (cf. wiggly)

                        && x.isInitialWellSet(cs));
    }

    @Override
    public Map<Role, Set<Role>> getStrongDeps() {
        Set<Role> rs = getRoles();
        Set<Map<Role, Set<Role>>> nested = this.cases.values().stream()
                .map(x -> x.getStrongDeps()).collect(Collectors.toSet());

        Map<Role, Set<Role>> res = GTUtil.mapOf();
        for (Role r : rs) {
            Iterator<Map<Role, Set<Role>>> it = nested.iterator();
            Map<Role, Set<Role>> fst = it.next();
            Set<Role> tmp = fst.containsKey(r) ? fst.get(r) : GTUtil.setOf();
            while (it.hasNext()) {
                Map<Role, Set<Role>> next = it.next();
                if (!next.containsKey(r)) {
                    tmp = GTUtil.setOf();
                    break;
                }
                tmp.retainAll(next.get(r));
            }
            if (r.equals(this.dst)) {
                tmp.add(this.src);
            } else if (tmp.contains(this.dst)) {
                tmp.add(this.src);
            }
            res.put(r, tmp);
        }

        return res;
    }

    @Override
    public boolean isSingleDecision(Theta theta) {
        return this.cases.values().stream().allMatch(x -> x.isSingleDecision(theta));
    }

    @Override
    public boolean isClearTermination() {
        return this.cases.values().stream().allMatch(GTGType::isClearTermination);
    }

    @Override
    public boolean isLeftCommitting(Set<Role> com, Set<Role> rem) {
        if (!com.contains(this.src) || rem.contains(this.dst)) {
            return this.cases.values().stream().allMatch(x -> x.isLeftCommitting(com, rem));
        }
        Set<Role> c_copy = GTUtil.copyOf(com);
        Set<Role> r_copy = GTUtil.copyOf(rem);
        c_copy.add(this.dst);
        r_copy.remove(this.dst);
        return this.cases.values().stream().allMatch(x -> x.isLeftCommitting(c_copy, r_copy));
    }

    @Override
    public boolean isLeftCommittingAux(Role obs, Set<Role> com, Set<Role> rem) {
        if (!rem.contains(this.dst) || !(obs.equals(this.dst) || com.contains(this.src))) {
            return this.cases.values().stream().allMatch(x -> x.isLeftCommittingAux(obs, com, rem));
        }
        Set<Role> c_copy = GTUtil.copyOf(com);
        Set<Role> r_copy = GTUtil.copyOf(rem);
        c_copy.add(this.dst);
        r_copy.remove(this.dst);
        return this.cases.values().stream().allMatch(x -> x.isLeftCommittingAux(obs, c_copy, r_copy));
    }

    /* ... */

    @Override
    public boolean isChoicePartip() {
        Collection<GTGType> cs = this.cases.values();
        if (cs.size() == 1) { return true; }

        // !!! cf. def 4
        Set<Role> fst = GTUtil.union(cs.iterator().next().getRoles(), Set.of(this.src, this.dst));

        // !!!
        return cs.stream().skip(1).anyMatch(x -> GTUtil.union(x.getRoles(), Set.of(this.src, this.dst)).equals(fst))
                && cs.stream().allMatch(GTGType::isChoicePartip);
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        return this.cases.values().stream().allMatch(x -> x.isUniqueInstan(seen));
    }

    @Override
    public boolean isAwareCorollary(GTSModelFactory mf, Theta theta) {
        return this.cases.values().stream().allMatch(x -> x.isAwareCorollary(mf, theta));
    }

    @Override
    public boolean isCoherent() {
        return this.cases.values().stream().allMatch(GTGType::isCoherent);
    }

    /* ... */

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (r.equals(this.src) || r.equals(this.dst)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            Sigma sigma = null;
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Optional<Pair<? extends GTLType, Sigma>> opt = e.getValue().project(rs, r, c, n);
                if (opt.isEmpty()) {
                    return Optional.empty();
                }

                // TODO factor out with merge case (reduce over sigmas)
                Pair<? extends GTLType, Sigma> p = opt.get();
                if (sigma == null) {
                    sigma = p.right;
                } else if (!sigma.equals(p.right)) {
                    return Optional.empty();
                }

                cases.put(e.getKey(), p.left);
            }
            return r.equals(this.src)
                    ? Optional.of(new Pair<>(lf.select(this.dst, cases), sigma))
                    : Optional.of(new Pair<>(lf.branch(this.src, cases), sigma));
        } else {
            Stream<Optional<Pair<? extends GTLType, Sigma>>> str =
                    this.cases.values().stream().map(x -> x.project(rs, r, c, n));
            Optional<Pair<? extends GTLType, Sigma>> fst = str.findFirst().get();  // Non-empty

            // FIXME stream made twice... -- refactor with GTGWiggly
            str = this.cases.values().stream().map(x -> x.project(rs, r, c, n));  // !!! XXX
            return str.skip(1).reduce(fst, GTGInteraction::mergePair);
        }
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        if (this.src.equals(r) || this.dst.equals(r)) {
            return Optional.of(new Theta(cs));
        }
        // FIXME refactor merge
        List<Optional<Theta>> distinct = this.cases.values().stream()
                .map(x -> x.projectTheta(cs, r)).distinct().collect(Collectors.toList());
        if (distinct.size() != 1) {
            return Optional.empty();
        }
        return distinct.get(0);
    }

    /* ... */

    // TODO refactor with GTMixedActive -- XXX mixed active needs to do Sigma.circ
    public static Optional<Pair<? extends GTLType, Sigma>> mergePair(
            Optional<Pair<? extends GTLType, Sigma>> left,
            Optional<Pair<? extends GTLType, Sigma>> right) {
        /*if (left.isEmpty() || right.isEmpty()) {
            return Optional.empty();
        }*/
        Optional<? extends GTLType> merge = merge(left.map(x -> x.left), right.map(x -> x.left));
        Optional<Sigma> sigma = mergeSigma(left.map(x -> x.right), right.map(x -> x.right));
        return merge.flatMap(x -> sigma.map(y -> new Pair<>(x, y)));  // nested `map` OK, result should be empty only when Opt is empty
    }

    public static Optional<Sigma> mergeSigma(
            Optional<Sigma> left, Optional<Sigma> right) {
        return left.flatMap(x ->
                right.flatMap(y ->
                        x.equals(y) ? Optional.of(x) : Optional.empty()));  // nested `flatMap`, result may be empty even if Opt not empty
    }

    // !!! TODO refactor with GTLType.merge
    public static Optional<? extends GTLType> merge(
            Optional<? extends GTLType> left, Optional<? extends GTLType> right) {
        /*if (left.isEmpty() || right.isEmpty()) {
            return Optional.empty();
        }
        GTLType l = left.get();
        GTLType r = right.get();
        if (l.equals(r)) {  // !!! TODO
            return left;
        } else {
            throw new RuntimeException("TODO");
        }*/
        return left.flatMap(x -> right.flatMap(x::merge));
    }

    /* ... */

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        if (this.src.equals(a.subj)) {
            if (a.isSend()) {  // [Snd]
                GTSSend<DynamicActionKind> cast = (GTSSend<DynamicActionKind>) a;
                if (cast.obj.equals(this.dst) && this.cases.containsKey(cast.mid)
                        && cast.c == c && cast.n == n) {
                    //return Optional.of(this.cases.get(cast.mid));
                    LinkedHashMap<Op, GTGType> tmp = new LinkedHashMap<>(this.cases);
                    GTGWiggly succ = this.fact.wiggly(this.src, this.dst, (Op) cast.mid, tmp);
                    return Either.right(new Triple<>(theta, succ, Tree.of(
                            toStepJudgeString("[Snd]", c, n, theta, this, cast, theta, succ))));
                }
            }
            return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
        } else if (!this.dst.equals(a.subj)) {  // [Cont1]
            /*return done
                ? Optional.of(this.fact.choice(this.src, this.dst, cs))
                : Optional.empty();*/
            Either<Exception, Triple<Theta, LinkedHashMap<Op, GTGType>, List<Tree<String>>>> nested =
                    stepNested(this.cases, theta, a, c, n);
            return nested.mapRight(x -> {
                GTGInteraction succ = this.fact.choice(this.src, this.dst, x.mid);
                return Triple.of(x.left, succ, Tree.of(
                        toStepJudgeString("[Cont1]", c, n, theta, this, (GTSAction) a, x.left, succ),
                        x.right));
            });
        }
        return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
    }

    protected Either<Exception, Triple<Theta, LinkedHashMap<Op, GTGType>, List<Tree<String>>>> stepNested(
            Map<Op, GTGType> cases, Theta theta, SAction<DynamicActionKind> a, int c1, int n1) {
        Set<Map.Entry<Op, GTGType>> es = cases.entrySet();
        Theta fst = null;
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        List<Tree<String>> trees = new LinkedList<>();
        for (Map.Entry<Op, GTGType> e : es) {
            Op op = e.getKey();
            GTGType c = e.getValue();
            Either<Exception, Triple<Theta, GTGType, Tree<String>>> step = c.step(theta, a, c1, n1);
            if (step.isLeft()) {
                return Either.left(step.getLeft());
            }
            Triple<Theta, GTGType, Tree<String>> p = step.getRight();
            if (fst == null) {
                fst = p.left;
            } else if (!p.left.equals(fst)) {
                return Either.left(new Exception("Thetas not mergeable: " + fst + ", " + p.left));
            }
            cs.put(op, p.mid);
            trees.add(p.right);
        }
        return Either.right(Triple.of(fst, cs, trees));
        /*boolean done = false;
        for (Map.Entry<Op, GTGType> e : es) {
            Op k = e.getKey();
            GTGType v = e.getValue();
            if (done) {  // ???
                cs.put(k, v);
            } else {
                Optional<GTGType> step = e.getValue().step(a);
                if (step.isPresent()) {
                    cs.put(k, step.get());
                    done = true;  // ???
                } else {
                    cs.put(k, v);
                }
            }
        }
        return done ? Optional.of(cs) : Optional.empty();*/
        //throw new RuntimeException("Shouldn't get here: " + cases + " ,, " + a);  // cases non-empty
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        //Stream.concat(blocked.stream(), Stream.of(this.src, this.dst)).collect(Collectors.toSet());
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.src);
        tmp.add(this.dst);
        ////this.cases.values().stream().flatMap(x -> x.getActs(tmp).stream()).collect(Collectors.toCollection(LinkedHashSet::new));
        //LinkedHashSet<SAction> collect = new LinkedHashSet<>();
        LinkedHashSet<SAction<DynamicActionKind>> res = new LinkedHashSet<>();

        Map<Op, LinkedHashSet<SAction<DynamicActionKind>>> coll = new LinkedHashMap<>();
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            if (!blocked.contains(this.src)) {
                SSend<DynamicActionKind> a = mf.GTSSend(this.src, this.dst, e.getKey(), Payload.EMPTY_PAYLOAD, c, n);  // FIXME empty
                res.add(a);
            }
            //collect.addAll(e.getValue().getActs(mf, theta, tmp));
            coll.put(e.getKey(), e.getValue().getActs(mf, theta, tmp, c, n));
        }

        // !!!
        Collection<LinkedHashSet<SAction<DynamicActionKind>>> vs = coll.values();
        for (SAction<DynamicActionKind> a : vs.iterator().next()) {
            if (vs.stream().allMatch(x -> x.contains(a))) {
                res.add(a);
            }
        }

        return res;
    }

    /* ... */

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> weakStep(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        return step(theta, a, c, n);
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>> getWeakActs(
            GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        return getActs(mf, theta, blocked, c, n);
    }

    /* ... */

    @Override
    public Set<Op> getCommittingTop(Set<Role> com) {
        Set<Op> res = GTUtil.setOf();
        this.cases.values()
                .forEach(x -> res.addAll(x.getCommittingTop(com)));
        return res;
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        Set<Op> res = GTUtil.setOf();
        Set<Role> com1 = GTUtil.copyOf(com);
        if ((this.dst.equals(obs) && !com.contains(obs))  // src doesn't need to be com, cf. below case
                || (com.contains(this.src) && !com.contains(this.dst))) {
            res.addAll(this.cases.keySet());
            com1.add(this.dst);
        }
        this.cases.values().stream()
                .forEach(x -> res.addAll(x.getCommittingLeft(obs, com1)));
        return res;
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        Set<Op> res = GTUtil.setOf();
        Set<Role> com1 = GTUtil.copyOf(com);
        if (!com.contains(this.src) && this.src.equals(obs)) {
            res.addAll(this.cases.keySet());
            com1.add(obs);
            com1.add(this.dst);
        } else if (com.contains(this.src) && !com.contains(this.dst)) {
            res.addAll(this.cases.keySet());
            com1.add(this.dst);
        }
        this.cases.values().stream()
                .forEach(x -> res.addAll(x.getCommittingRight(obs, com1)));
        return res;
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        Map<Op, Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>>> collect =
                this.cases.entrySet().stream().collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().getLabels()
                ));
        Set<Op> imm = GTUtil.setOf();
        Map<Integer, Pair<Set<Op>, Set<Op>>> nested = GTUtil.mapOf();
        for (Map.Entry<Op, GTGType> x : this.cases.entrySet()) {
            Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> tmp = x.getValue().getLabels();
            imm.add(x.getKey());
            imm.addAll(tmp.left);
            if (nested.keySet().stream().anyMatch(y -> tmp.right.keySet().contains(y))) {
                throw new RuntimeException("Shouldn't get here: " + this + " ,, " + tmp);
            }
            nested.putAll(tmp.right);
        }
        return Pair.of(imm, nested);
    }

    /* Aux */

    @Override
    public GTGInteraction subs(Map<RecVar, GTGType> subs) {
        LinkedHashMap<Op, GTGType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().subs(subs),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return new GTGInteraction(this.src, this.dst, cases);
    }

    @Override
    public GTGInteraction unfoldAllOnce() {
        return this;
    }

    @Override
    public Set<Role> getRoles() {
        return Stream.concat(Stream.of(this.src, this.dst),
                        this.cases.values().stream().flatMap(x -> x.getRoles().stream()))
                .collect(Collectors.toSet());
    }

    public Role getSender() {
        return this.src;
    }

    public Role getReceiver() {
        return this.dst;
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.cases.values().stream()
                .flatMap(x -> x.getTimeoutIds().stream())
                .collect(Collectors.toSet());
    }

    @Override
    public Set<Op> getOps() {
        Set<Op> ops = new HashSet<>(this.cases.keySet());
        this.cases.values().forEach(x -> ops.addAll(x.getOps()));
        return ops;
    }

    @Override
    public String toString() {
        return this.src + "->" + this.dst
                + "{" + this.cases.entrySet().stream()
                .map(e -> e.getKey() + "." + e.getValue())
                .collect(Collectors.joining(", ")) + "}";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.CHOICE_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGInteraction)) { return false; }
        GTGInteraction them = (GTGInteraction) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.dst.equals(them.dst)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGInteraction;
    }
}
