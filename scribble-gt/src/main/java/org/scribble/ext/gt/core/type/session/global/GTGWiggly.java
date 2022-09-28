package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.global.SModelFactory;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GTGWiggly implements GTGType {

    private final GTGTypeFactory fact = GTGTypeFactory.FACTORY;

    // TODO factor out with GTGChoice (and locals)
    public final Role src;
    public final Role dst;
    public final Op op;  // Pre: this.cases.containsKey(this.op)
    public final Map<Op, GTGType> cases;

    protected GTGWiggly(Role src, Role dst, Op op, LinkedHashMap<Op, GTGType> cases) {
        this.src = src;
        this.dst = dst;
        this.op = op;
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public Optional<? extends GTLType> project(Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (this.src.equals(r)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            /*for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Optional<? extends GTLType> p = e.getValue().project(r);
                if (p.isEmpty()) {
                    return Optional.empty();
                }
                cases.put(e.getKey(), p.get());
            }
            return Optional.of(lf.select(this.dst, cases));*/  // !!!
            Optional<? extends GTLType> p = this.cases.get(this.op).project(r);
            if (p.isEmpty()) {
                return Optional.empty();
            }
            cases.put(this.op, p.get());
            return Optional.of(lf.select(this.dst, cases));  // !!! CHECKME
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
            return str.skip(1).reduce(fst, GTGInteraction::merge);
        }
    }

    @Override
    public boolean isSinglePointed() {
        /*Set<Op> labs1 = new HashSet<>(labs);
        labs1.addAll(this.cases.keySet());
        if (labs1.size() != labs.size() + this.cases.keySet().size()) {
            return false;
        }
        return this.cases.values().stream().allMatch(x -> x.isStaticWF(labs1));*/
        throw new RuntimeException("N/A");
    }

    @Override
    public boolean isGood() {
        return this.cases.values().stream().allMatch(GTGType::isGood)
                && this.cases.keySet().contains(this.op);  // !!!
    }

    @Override
    public boolean isCoherent() {
        return this.cases.values().stream().allMatch(x -> x.isCoherent());
    }

    @Override
    public Optional<GTGType> step(SAction a) {
        if (this.dst.equals(a.subj)) {
            if (a.isReceive()) {  // [Rcv]
                SRecv cast = (SRecv) a;
                if (cast.obj.equals(this.src)
                        && this.cases.keySet().contains(cast.mid)) {
                    //return Optional.of(this.cases.get(cast.mid));
                    LinkedHashMap<Op, GTGType> tmp = new LinkedHashMap<>(this.cases);
                    return Optional.of(this.cases.get(cast.mid));
                }
            }
            return Optional.empty();
        } else {  // [Cont2]
            /*return done
                    ? Optional.of(this.fact.wiggly(this.src, this.dst, this.op, cs))
                    : Optional.empty();*/
            Optional<LinkedHashMap<Op, GTGType>> nestedCases
                    = GTGInteraction.stepNested(this.cases, a);  // !!! XXX I\k
            return nestedCases.map(x -> this.fact.wiggly(this.src, this.dst, this.op, x));
        }
    }

    @Override
    public LinkedHashSet<SAction> getActs(SModelFactory mf, Set<Role> blocked) {
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.dst);
        LinkedHashSet<SAction> collect = new LinkedHashSet<>();
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            if (!blocked.contains(this.dst)) {
                // N.B. SRecv subj is this.dst
                SRecv a = mf.SRecv(this.dst, this.src, e.getKey(), Payload.EMPTY_PAYLOAD);  // FIXME empty
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
        ops.add(this.op);  // Not assuming single-pointedness
        return ops;
    }

    /* Aux */

    @Override
    public String toString() {
        return this.src + "~>" + this.dst + ":" + this.op
                + "{" + this.cases.entrySet().stream()
                .map(e -> e.getKey() + "." + e.getValue())
                .collect(Collectors.joining(", ")) + "}";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTGType.WIGGLY_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTGWiggly)) return false;
        GTGWiggly them = (GTGWiggly) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.dst.equals(them.dst)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTGWiggly;
    }
}
