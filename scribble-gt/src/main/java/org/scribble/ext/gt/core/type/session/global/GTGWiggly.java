package org.scribble.ext.gt.core.type.session.global;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.MActionBase;
import org.scribble.core.model.global.actions.SAction;
import org.scribble.core.model.global.actions.SRecv;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.global.GTSModelFactory;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.local.GTLType;
import org.scribble.ext.gt.core.type.session.local.GTLTypeFactory;
import org.scribble.ext.gt.util.Triple;
import org.scribble.util.Pair;

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
    public GTGWiggly unfoldContext(Map<RecVar, GTGType> c) {
        LinkedHashMap<Op, GTGType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().unfoldContext(c),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return new GTGWiggly(this.src, this.dst, this.op, cases);
    }

    @Override
    public Set<Integer> getTimeoutIds() {
        return this.cases.values().stream()
                .flatMap(x -> x.getTimeoutIds().stream())
                .collect(Collectors.toSet());
    }

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> rs, Role r) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (r.equals(this.src)) {
            //LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            return this.cases.get(this.op).project(rs, r);
        } else if (r.equals(this.dst)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            Sigma sigma = null;
            Sigma sigma_k = null;
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Op op = e.getKey();
                Optional<Pair<? extends GTLType, Sigma>> opt = e.getValue().project(rs, r);
                if (opt.isEmpty()) {
                    return Optional.empty();
                }
                Pair<? extends GTLType, Sigma> p = opt.get();
                if (op.equals(this.op)) {
                    sigma_k = p.right;
                } else {
                    if (sigma == null) {
                        sigma = p.right;
                    } else if (!sigma.equals(p.right)) {
                        return Optional.empty();
                    }
                }
                cases.put(op, p.left);
            }
            Map<Role, List<GTESend<DynamicActionKind>>> tmp = new LinkedHashMap<>(sigma_k.map);
            List<GTESend<DynamicActionKind>> as = tmp.containsKey(this.src)
                    ? new LinkedList<>(tmp.get(this.src))
                    : new LinkedList<>();

            System.err.println("[WARNING] TODO local actions");
            GTESend<DynamicActionKind> m =
                    new GTESend<>(MActionBase.DYNAMIC_ID, null, this.dst,
                            //this.op, Payload.EMPTY_PAYLOAD, CCC, NNN);  // HERE HERE projection needs c, n  // FIXME mf/ef?
                            this.op, Payload.EMPTY_PAYLOAD, -1, 0);  // HERE HERE projection needs c, n  // FIXME mf/ef?
            as.add(0, m);

            tmp.put(this.src, as);
            sigma_k = new Sigma(tmp);
            return Optional.of(new Pair<>(lf.branch(this.src, cases), sigma_k));
        } else {
            Stream<Optional<Pair<? extends GTLType, Sigma>>> str =
                    this.cases.values().stream().map(x -> x.project(rs, r));
            Optional<Pair<? extends GTLType, Sigma>> fst = str.findFirst().get();  // Non-empty

            // FIXME stream made twice... -- duplicated from GTGInteraction
            str = this.cases.values().stream().map(x -> x.project(rs, r));  // !!! XXX
            return str.skip(1).reduce(fst, GTGInteraction::mergePair);
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
                && this.cases.containsKey(this.op);  // !!!
    }

    @Override
    public boolean isCoherent() {
        return this.cases.values().stream().allMatch(GTGType::isCoherent);
    }

    @Override
    public Optional<Triple<Theta, GTGType, String>> step(Theta theta, SAction<DynamicActionKind> a) {
        if (this.dst.equals(a.subj)) {
            if (a.isReceive()) {  // [Rcv]
                SRecv<?> cast = (SRecv<?>) a;
                if (cast.obj.equals(this.src)
                        && this.cases.containsKey(cast.mid)) {
                    //return Optional.of(this.cases.get(cast.mid));
                    LinkedHashMap<Op, GTGType> tmp = new LinkedHashMap<>(this.cases);
                    return Optional.of(new Triple<>(theta, this.cases.get(cast.mid), "[Rcv]"));
                }
            }
            return Optional.empty();
        } else {  // [Cont2]
            /*return done
                    ? Optional.of(this.fact.wiggly(this.src, this.dst, this.op, cs))
                    : Optional.empty();*/
            Optional<Triple<Theta, LinkedHashMap<Op, GTGType>, String>> nestedCases
                    = stepNested(theta, a);
            return nestedCases.map(x -> new Triple<>(
                    x.left,
                    this.fact.wiggly(this.src, this.dst, this.op, x.mid),
                    "[Cont2]" + x.right));
        }
    }

    protected Optional<Triple<Theta, LinkedHashMap<Op, GTGType>, String>> stepNested(
            Theta theta, SAction<DynamicActionKind> a) {
        /*//Set<Map.Entry<Op, GTGType>> es = cases.entrySet();
        LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>();
        for (Map.Entry<Op, GTGType> e : es) {
            Op op = e.getKey();
            GTGType c = e.getValue();
            if (op.equals(this.op)) {
                Optional<Pair<Theta, GTGType>> step = c.step(theta, a);
                if (step.isEmpty()) {
                    return Optional.empty();
                }
                cs.put(op, step.get().right);
            } else {
                cs.put(op, c);
            }
        }
        return Optional.of(cs);*/
        Optional<Triple<Theta, GTGType, String>> step = this.cases.get(this.op).step(theta, a);
        if (step.isPresent()) {
            Triple<Theta, GTGType, String> get = step.get();
            LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>(this.cases);
            cs.put(op, get.mid);
            return Optional.of(new Triple<>(get.left, cs, get.right));
        }
        return Optional.empty();
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>>
    getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked) {
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.dst);
        LinkedHashSet<SAction<DynamicActionKind>> res = new LinkedHashSet<>();

        Map<Op, LinkedHashSet<SAction<DynamicActionKind>>> coll = new LinkedHashMap<>();  // no subj=this.dst due to tmp (blocked)
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            Op op = e.getKey();
            LinkedHashSet<SAction<DynamicActionKind>> as = e.getValue().getActs(mf, theta, tmp);
            coll.put(op, new LinkedHashSet<>(
                    as.stream().filter(x -> !x.subj.equals(this.src)).collect(Collectors.toSet())));
        }

        if (!blocked.contains(this.dst)) {
            // N.B. SRecv subj is this.dst
            SRecv<DynamicActionKind> a = mf.SRecv(this.dst, this.src, this.op, Payload.EMPTY_PAYLOAD);  // FIXME empty
            res.add(a);
        }
        this.cases.get(this.op).getActs(mf, theta, blocked).stream()
                .filter(x -> x.subj.equals(this.src)).forEach(x -> res.add(x));

        // !!!
        Collection<LinkedHashSet<SAction<DynamicActionKind>>> vs = coll.values();
        for (SAction<DynamicActionKind> a : vs.iterator().next()) {
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
