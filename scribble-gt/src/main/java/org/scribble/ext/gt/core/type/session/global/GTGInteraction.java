package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.actions.SSend;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTGInteraction implements GTGType {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    public final Role src;
    public final Role dst;
    public final Map<Op, GTGType> cases;  // Pre: Unmodifiable, non-empty

    protected GTGInteraction(Role src, Role dst, LinkedHashMap<Op, GTGType> cases) {
        this.src = src;
        this.dst = dst;
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                        Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                                (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public Optional<? extends GTLType> project(Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (this.src.equals(r)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Optional<? extends GTLType> p = e.getValue().project(r);
                if (p.isEmpty()) {
                    return Optional.empty();
                }
                cases.put(e.getKey(), p.get());
            }
            return Optional.of(lf.select(this.dst, cases));
        } else if (this.dst.equals(r)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Optional<? extends GTLType> p = e.getValue().project(r);
                if (p.isEmpty()) {
                    return Optional.empty();
                }
                cases.put(e.getKey(), p.get());
            }
            return Optional.of(lf.branch(this.src, cases));
        } else {
            Stream<Optional<? extends GTLType>> str =
                    this.cases.values().stream().map(x -> x.project(r));
            Optional<? extends GTLType> fst = str.findFirst().get();  // Non-empty

            str = this.cases.values().stream().map(x -> x.project(r));  // !!! XXX
            return str.skip(1).reduce(fst, GTGInteraction::merge);
        }
    }

    protected static Optional<? extends GTLType> merge(
            Optional<? extends GTLType> left, Optional<? extends GTLType> right) {
        if (left.isEmpty() || right.isEmpty()) {
            return Optional.empty();
        }
        GTLType l = left.get();
        GTLType r = right.get();
        if (l.equals(r)) {  // !!! TODO
            return left;
        } else {
            throw new RuntimeException("TODO");
        }
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
    public Optional<GTGType> step(SAction a) {
        if (this.src.equals(a.subj)) {
           if (a.isSend()) {  // [Snd]
               SSend cast = (SSend) a;
               if (cast.obj.equals(this.dst)
                       && this.cases.keySet().contains(cast.mid)) {
                   //return Optional.of(this.cases.get(cast.mid));
                   LinkedHashMap<Op, GTGType> tmp = new LinkedHashMap<>(this.cases);
                   return Optional.of(this.fact.wiggly(this.src, this.dst, (Op) cast.mid, tmp));
               }
           }
           return Optional.empty();
        } else if (!this.dst.equals(a.subj)) {  // [Cont1]
            /*return done
                ? Optional.of(this.fact.choice(this.src, this.dst, cs))
                : Optional.empty();*/
            Optional<LinkedHashMap<Op, GTGType>> nestedCases = stepNested(this.cases, a);
            return nestedCases.map(x -> this.fact.choice(this.src, this.dst, x));
        }
        return Optional.empty();
    }

    protected static Optional<LinkedHashMap<Op, GTGType>> stepNested(
            Map<Op, GTGType> cases, SAction a) {
        Set<Map.Entry<Op, GTGType>> es = cases.entrySet();
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        for (Map.Entry<Op, GTGType> e : es) {
            Op op = e.getKey();
            GTGType c = e.getValue();
            Optional<GTGType> step = c.step(a);
            if (step.isEmpty()) {
                return Optional.empty();
            }
            cs.put(op, step.get());
        }
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
        throw new RuntimeException("Shouldn't get here");  // cases non-empty
    }

    @Override
    public LinkedHashSet<SAction> getActs(SModelFactory mf, Set<Role> blocked) {
        //Stream.concat(blocked.stream(), Stream.of(this.src, this.dst)).collect(Collectors.toSet());
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.src);
        tmp.add(this.dst);
        //this.cases.values().stream().flatMap(x -> x.getActs(tmp).stream()).collect(Collectors.toCollection(LinkedHashSet::new));
        LinkedHashSet<SAction> collect = new LinkedHashSet<>();
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            if (!blocked.contains(this.src)) {
                SSend a = mf.SSend(this.src, this.dst, e.getKey(), Payload.EMPTY_PAYLOAD);  // FIXME empty
                collect.add(a);
            }
            collect.addAll(e.getValue().getActs(mf, tmp));
        }
        return collect;
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
