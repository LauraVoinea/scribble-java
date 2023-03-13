package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.endpoint.EModelFactory;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.type.session.global.GTGInteraction;
import org.scribble.ext.gt.core.type.session.global.GTGType;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTLBranch implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final Role src;
    public final Map<Op, GTLType> cases;  // Pre: Unmodifiable

    protected GTLBranch(Role src, LinkedHashMap<Op, GTLType> cases) {
        this.src = src;
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }

    @Override
    public GTLBranch unfoldContext(Map<RecVar, GTLType> env) {
        LinkedHashMap<Op, GTLType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().unfoldContext(env),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return this.fact.branch(this.src, cases);
    }

    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLBranch)) {
            return Optional.empty();
        }
        GTLBranch cast = (GTLBranch) t;
        if (!this.src.equals(cast.src)) {
            return Optional.empty();
        }
        LinkedHashMap<Op, GTLType> tmp = new LinkedHashMap<>();
        Iterator<Op> it = Stream.of(this.cases.keySet(), cast.cases.keySet()).flatMap(Collection::stream).iterator();
        while (it.hasNext()) {
            Op x = it.next();
            if (this.cases.keySet().contains(x)) {  // Would be nice if get returned Optional... can map empty directly
                if (cast.cases.keySet().contains(x)) {
                    Optional<? extends GTLType> opt = this.cases.get(x).merge(cast.cases.get(x));
                    if (!opt.isPresent()) {
                        return Optional.empty();
                    }
                    tmp.put(x, opt.get());
                } else {
                    tmp.put(x, this.cases.get(x));
                }
            } else { //if (cast.cases.keySet().contains(x)) {
                tmp.put(x, cast.cases.get(x));
            }
        }
        return Optional.of(this.fact.branch(this.src, tmp));
    }

    @Override
    public Optional<GTLType> step(EAction a) {
        /*if (a.subj.equals(this.src)) {
           if (a.isSend()) {
               SSend cast = (SSend) a;
               if (cast.obj.equals(this.dst)
                       && this.cases.keySet().contains(cast.mid)) {
                   //return Optional.of(this.cases.get(cast.mid));
                   LinkedHashMap<Op, GTLType> tmp = new LinkedHashMap<>(this.cases);
                   return Optional.of(this.fact.wiggly(this.src, this.dst, (Op) cast.mid, tmp));
               }
           }
           return Optional.empty();
        } else {
            /*return done
                ? Optional.of(this.fact.choice(this.src, this.dst, cs))
                : Optional.empty();* /
            Optional<LinkedHashMap<Op, GTLType>> nestedCases = stepCases(this.cases, a);
            return nestedCases.map(x -> this.fact.choice(this.src, this.dst, x));
        }*/
        throw new RuntimeException("TODO");
    }

    /*protected static Optional<LinkedHashMap<Op, GTLType>> stepCases(
            Map<Op, GTLType> cases, SAction a) {
        Set<Map.Entry<Op, GTLType>> es = cases.entrySet();
        LinkedHashMap<Op, GTLType> cs = new LinkedHashMap<>();
        boolean done = false;
        for (Map.Entry<Op, GTLType> e : es) {
            Op k = e.getKey();
            GTLType v = e.getValue();
            if (done) {
                cs.put(k, v);
            } else {
                Optional<GTLType> step = e.getValue().step(a);
                if (step.isPresent()) {
                    cs.put(k, step.get());
                    done = true;
                } else {
                    cs.put(k, v);
                }
            }
        }
        return done ? Optional.of(cs) : Optional.empty();
    }*/

    @Override
    public LinkedHashSet<EAction> getActs(EModelFactory mf, Set<Role> blocked) {
        /*//Stream.concat(blocked.stream(), Stream.of(this.src, this.dst)).collect(Collectors.toSet());
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.src);
        tmp.add(this.dst);
        //this.cases.values().stream().flatMap(x -> x.getActs(tmp).stream()).collect(Collectors.toCollection(LinkedHashSet::new));
        LinkedHashSet<SAction> collect = new LinkedHashSet<>();
        for (Map.Entry<Op, GTLType> e : this.cases.entrySet()) {
            if (!blocked.contains(this.src)) {
                SSend a = mf.SSend(this.src, this.dst, e.getKey(), Payload.EMPTY_PAYLOAD);  // FIXME empty
                collect.add(a);
            }
            collect.addAll(e.getValue().getActs(mf, tmp));
        }
        return collect;*/
        throw new RuntimeException("TODO");
    }

    /* Aux */

    @Override
    public String toString() {
        return this.src + "& {"
                + this.cases.entrySet().stream()
                .map(e -> e.getKey() + "." + e.getValue())
                .collect(Collectors.joining(", "))
                + "}";
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.BRANCH_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) return true;
        if (obj == null || !(obj instanceof GTLBranch)) return false;
        GTLBranch them = (GTLBranch) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLBranch;
    }
}
