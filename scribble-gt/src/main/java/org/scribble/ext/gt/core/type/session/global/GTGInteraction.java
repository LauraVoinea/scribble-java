package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.actions.SSend;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
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

    @Override
    public GTGInteraction unfoldContext(Map<RecVar, GTGType> c) {
        LinkedHashMap<Op, GTGType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().unfoldContext(c),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return new GTGInteraction(this.src, this.dst, cases);
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.cases.values().stream()
                .flatMap(x -> x.getTimeoutIds().stream())
                .collect(Collectors.toSet());
    }

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (r.equals(this.src) || r.equals(this.dst)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            Sigma sigma = null;
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Optional<Pair<? extends GTLType, Sigma>> opt = e.getValue().project(r);
                if (opt.isEmpty()) {
                    return Optional.empty();
                }
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
                    this.cases.values().stream().map(x -> x.project(r));
            Optional<Pair<? extends GTLType, Sigma>> fst = str.findFirst().get();  // Non-empty

            // FIXME stream made twice... -- refactor with GTGWiggly
            str = this.cases.values().stream().map(x -> x.project(r));  // !!! XXX
            return str.skip(1).reduce(fst, GTGInteraction::mergePair);
        }
    }

    // TODO refactor with GTMixedActive
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
        return left.flatMap(x -> right.flatMap(y -> x.merge(y)));
    }

    @Override
    public boolean isSinglePointed() {
        return this.cases.values().stream().allMatch(GTGType::isSinglePointed);
    }

    @Override
    public boolean isGood() {
        return this.cases.values().stream().allMatch(GTGType::isGood);
    }

    @Override
    public boolean isCoherent() {
        return this.cases.values().stream().allMatch(x -> x.isCoherent());
    }

    @Override
    public Optional<Pair<Theta, GTGType>> step(Theta theta, SAction a) {
        if (this.src.equals(a.subj)) {
            if (a.isSend()) {  // [Snd]
                SSend cast = (SSend) a;
                if (cast.obj.equals(this.dst)
                        && this.cases.keySet().contains(cast.mid)) {
                    //return Optional.of(this.cases.get(cast.mid));
                    LinkedHashMap<Op, GTGType> tmp = new LinkedHashMap<>(this.cases);
                    return Optional.of(new Pair<>(
                            theta,
                            this.fact.wiggly(this.src, this.dst, (Op) cast.mid, tmp)));
                }
            }
            return Optional.empty();
        } else if (!this.dst.equals(a.subj)) {  // [Cont1]
            /*return done
                ? Optional.of(this.fact.choice(this.src, this.dst, cs))
                : Optional.empty();*/
            Optional<Pair<Theta, LinkedHashMap<Op, GTGType>>> nestedCases =
                    stepNested(this.cases, theta, a);
            return nestedCases.map(x -> new Pair<>(
                    x.left,
                    this.fact.choice(this.src, this.dst, x.right)));
        }
        return Optional.empty();
    }

    protected Optional<Pair<Theta, LinkedHashMap<Op, GTGType>>> stepNested(
            Map<Op, GTGType> cases, Theta theta, SAction a) {
        Set<Map.Entry<Op, GTGType>> es = cases.entrySet();
        Theta fst = null;
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        for (Map.Entry<Op, GTGType> e : es) {
            Op op = e.getKey();
            GTGType c = e.getValue();
            Optional<Pair<Theta, GTGType>> step = c.step(theta, a);
            if (step.isEmpty()) {
                return Optional.empty();
            }
            Pair<Theta, GTGType> p = step.get();
            if (fst == null) {
                fst = p.left;
            } else if (!p.left.equals(fst)) {
                return Optional.empty();
            }
            cs.put(op, p.right);
        }
        return Optional.of(new Pair<>(fst, cs));
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

    // HERE HERE HERE SAction generics -> need GT SAction with

    @Override
    public LinkedHashSet<SAction> getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked) {
        //Stream.concat(blocked.stream(), Stream.of(this.src, this.dst)).collect(Collectors.toSet());
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.src);
        tmp.add(this.dst);
        ////this.cases.values().stream().flatMap(x -> x.getActs(tmp).stream()).collect(Collectors.toCollection(LinkedHashSet::new));
        //LinkedHashSet<SAction> collect = new LinkedHashSet<>();
        LinkedHashSet<SAction> res = new LinkedHashSet<>();

        Map<Op, LinkedHashSet<SAction>> coll = new LinkedHashMap<>();
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            if (!blocked.contains(this.src)) {
                SSend a = mf.SSend(this.src, this.dst, e.getKey(), Payload.EMPTY_PAYLOAD);  // FIXME empty
                res.add(a);
            }
            //collect.addAll(e.getValue().getActs(mf, theta, tmp));
            coll.put(e.getKey(), e.getValue().getActs(mf, theta, tmp));
        }

        // !!!
        Collection<LinkedHashSet<SAction>> vs = coll.values();
        for (SAction a : vs.iterator().next()) {
            if (vs.stream().allMatch(x -> x.contains(a))) {
                res.add(a);
            }
        }

        return res;
    }

    @Override
    public Set<Op> getOps() {
        Set<Op> ops = new HashSet<>(this.cases.keySet());
        this.cases.values().forEach(x -> ops.addAll(x.getOps()));
        return ops;
    }

    /* Aux */

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
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGInteraction)) return false;
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
