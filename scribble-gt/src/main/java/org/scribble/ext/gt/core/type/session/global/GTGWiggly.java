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
import org.scribble.ext.gt.core.model.global.action.GTSAction;
import org.scribble.ext.gt.core.model.global.action.GTSRecv;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTESend;
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

    /* ... */

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

    /* ... */

    @Override
    public boolean isInitial() {
        return false;
    }

    @Override
    public boolean isInitialWellSet(Set<Integer> cs) {
        return false;
    }

    @Override
    public Map<Role, Set<Role>> getStrongDeps() {
        Set<Role> rs = getRoles();
        Set<Map<Role, Set<Role>>> nested = this.cases.values().stream()
                .map(GTGType::getStrongDeps).collect(Collectors.toSet());

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
            /* //  Now wiggly, cf. interaction
            if (r.equals(this.dst)) {
                tmp.add(this.src);
            } else*if (tmp.contains(this.dst)) {
                tmp.add(this.src);
            }*/
            res.put(r, tmp);
        }

        return res;
    }

    @Override
    public boolean isSingleDecision(Set<Role> top, Theta theta) {
        return this.cases.values().stream().allMatch(x -> x.isSingleDecision(top, theta));
    }

    @Override
    public boolean isClearTermination() {
        return this.cases.values().stream().allMatch(GTGType::isClearTermination);
    }

    @Override
    public boolean isLeftCommitting(Set<Role> com, Set<Role> rem) {
        /*if (!com.contains(this.src) || rem.contains(this.dst)) {
            return this.cases.values().stream().allMatch(x -> x.isLeftCommitting(com, rem));
        }
        Set<Role> c_copy = GTUtil.copyOf(com);
        Set<Role> r_copy = GTUtil.copyOf(rem);
        c_copy.add(this.dst);
        r_copy.remove(this.dst);
        return this.cases.values().stream().allMatch(x -> x.isLeftCommitting(c_copy, r_copy));*/
        throw new RuntimeException("Shouldn't get here: " + this);
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
        //System.out.println("2222: " + this + " ,, " + c_copy + "\n " + this.cases.values().stream().allMatch(x -> x.isLeftCommittingAux(obs, c_copy, r_copy)));
        return this.cases.values().stream().allMatch(x -> x.isLeftCommittingAux(obs, c_copy, r_copy));
    }

    /* ... */

    @Override
    public boolean isChoicePartip() {
        return this.cases.get(this.op).isChoicePartip();
    }

    @Override
    public boolean isUniqueInstan(Set<Pair<Integer, Integer>> seen) {
        return this.cases.values().stream().allMatch(x -> x.isUniqueInstan(seen));
    }

    @Override
    public boolean isAwareCorollary(GTSModelFactory mf, Set<Role> top, Theta theta) {
        return this.cases.values().stream().allMatch(x -> x.isAwareCorollary(mf, top, theta));
    }

    @Override
    public boolean isCoherent() {
        return this.cases.values().stream().allMatch(GTGType::isCoherent);
    }

    /* ... */

    @Override
    public Optional<Pair<? extends GTLType, Sigma>> project(Set<Role> topPeers, Role r, int c, int n) {
        GTLTypeFactory lf = GTLTypeFactory.FACTORY;
        if (r.equals(this.src)) {
            //LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            return this.cases.get(this.op).project(topPeers, r, c, n);
        } else if (r.equals(this.dst)) {
            LinkedHashMap<Op, GTLType> cases = new LinkedHashMap<>();
            Sigma sigma = null;
            Sigma sigma_k = null;
            for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
                Op op = e.getKey();
                Optional<Pair<? extends GTLType, Sigma>> opt = e.getValue().project(topPeers, r, c, n);
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

            GTESend<DynamicActionKind> m =
                    new GTESend<>(MActionBase.DYNAMIC_ID, null, this.dst,
                            this.op, Payload.EMPTY_PAYLOAD, c, n);  // FIXME mf/ef?
            as.add(0, m);

            tmp.put(this.src, as);
            sigma_k = new Sigma(tmp);
            return Optional.of(new Pair<>(lf.branch(this.src, cases), sigma_k));
        } else {
            /*Stream<Optional<Pair<? extends GTLType, Sigma>>> str =
                    this.cases.values().stream().map(x -> x.project(rs, r, c, n));
            Optional<Pair<? extends GTLType, Sigma>> fst = str.findFirst().get();  // Non-empty

            // FIXME stream made twice... -- duplicated from GTGInteraction
            str = this.cases.values().stream().map(x -> x.project(rs, r, c, n));  // !!! XXX
            return str.skip(1).reduce(fst, GTGInteraction::mergePair);*/

            Map<Op, Optional<Pair<? extends GTLType, Sigma>>> map =
                    this.cases.entrySet().stream().collect(Collectors.toMap(
                            Map.Entry::getKey,
                            x -> x.getValue().project(topPeers, r, c, n)
                    ));

            /*List<Optional<? extends GTLType>> ts = map.values().stream().map(x -> x.map(y -> y.left)).collect(Collectors.toList());
            Optional<? extends GTLType> fst = ts.get(0);
            Optional<? extends GTLType> merge = ts.stream().skip(1).reduce(fst, GTGInteraction::merge);

            Optional<Sigma> sig_k = map.get(this.op).map(x -> x.right);
            Optional<Pair<? extends GTLType, Sigma>> res = merge.flatMap(x -> sig_k.map(z -> Pair.of(x, z)));*/

            List<Optional<? extends GTLType>> ts = map.entrySet().stream()
                    .filter(x -> !x.getKey().equals(this.op))
                    .map(x -> x.getValue().map(y -> y.left))
                    .collect(Collectors.toList());
            if (!ts.isEmpty()) {
                Optional<? extends GTLType> fst = ts.get(0);
                Optional<? extends GTLType> merge = ts.stream().skip(1).reduce(fst, GTGInteraction::merge);
                if (merge.isEmpty()) {
                    return Optional.empty();
                }
            }
            Optional<Pair<? extends GTLType, Sigma>> res = map.get(this.op);

            List<Optional<Sigma>> filt = map.entrySet().stream().filter(x -> !x.getKey().equals(this.op))
                    .map(x -> x.getValue().map(y -> y.right)).collect(Collectors.toList());
            if (filt.size() == 0) {
                return res;
            } else {
                Optional<Sigma> fstsig = filt.get(0);
                Optional<Sigma> reduce = filt.stream().skip(1).reduce(fstsig, GTGInteraction::mergeSigma);
                return reduce.flatMap(y -> res);
            }
        }
    }

    @Override
    public Optional<Theta> projectTheta(Set<Integer> cs, Role r) {
        if (this.src.equals(r)) {
            return this.cases.get(this.op).projectTheta(cs, r);
        } else if (this.dst.equals(r)) {
            return Optional.of(new Theta(cs));
        } else {
            // FIXME refactor merge
            List<Optional<Theta>> distinct = this.cases.values().stream()
                    .map(x -> x.projectTheta(cs, r)).distinct().collect(Collectors.toList());
            if (distinct.size() != 1) {
                return Optional.empty();
            }
            return distinct.get(0);
        }
    }

    /* */

    @Override
    public Either<Exception, Triple<Theta, GTGType, Tree<String>>> step(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        if (this.dst.equals(a.subj)) {
            if (a.isReceive()) {  // [Rcv]
                GTSRecv<DynamicActionKind> cast = (GTSRecv<DynamicActionKind>) a;
                if (cast.obj.equals(this.src)
                        && this.cases.containsKey(cast.mid) && cast.c == c && cast.n == n) {
                    LinkedHashMap<Op, GTGType> tmp = new LinkedHashMap<>(this.cases);
                    GTGType succ = this.cases.get(cast.mid);
                    return Either.right(Triple.of(
                            theta, succ, Tree.of(toStepJudgeString(
                                    "[Rcv]", c, n, theta, this, cast, theta, succ))));
                }
            }
            return Either.left(newStuck(c, n, theta, this, (GTSAction) a));
        } else {  // [Cont2]
            Either<Exception, Triple<Theta, LinkedHashMap<Op, GTGType>, Tree<String>>> nested
                    = stepNested(theta, a, c, n);
            return nested.mapRight(x -> {
                GTGWiggly succ = this.fact.wiggly(this.src, this.dst, this.op, x.mid);
                return Triple.of(x.left, succ, Tree.of(
                        toStepJudgeString("[Cont2]", c, n, theta, this, (GTSAction) a, x.left, succ),
                        x.right));
            });
        }
    }

    protected Either<Exception, Triple<Theta, LinkedHashMap<Op, GTGType>, Tree<String>>> stepNested(
            Theta theta, SAction<DynamicActionKind> a, int c, int n) {
        Either<Exception, Triple<Theta, GTGType, Tree<String>>> step =
                this.cases.get(this.op).step(theta, a, c, n);
        return step.mapRight(x -> {
            LinkedHashMap<Op, GTGType> cs = new LinkedHashMap<>(this.cases);
            cs.put(op, x.mid);
            return Triple.of(x.left, cs, x.right);
        });
    }

    @Override
    public LinkedHashSet<SAction<DynamicActionKind>>
    getActs(GTSModelFactory mf, Theta theta, Set<Role> blocked, int c, int n) {
        HashSet<Role> tmp = new HashSet<>(blocked);
        tmp.add(this.dst);
        LinkedHashSet<SAction<DynamicActionKind>> res = new LinkedHashSet<>();

        Map<Op, LinkedHashSet<SAction<DynamicActionKind>>> coll = new LinkedHashMap<>();  // no subj=this.dst due to tmp (blocked)
        for (Map.Entry<Op, GTGType> e : this.cases.entrySet()) {
            Op op = e.getKey();
            LinkedHashSet<SAction<DynamicActionKind>> as = e.getValue().getActs(mf, theta, tmp, c, n);
            coll.put(op, new LinkedHashSet<>(
                    as.stream().filter(x -> !x.subj.equals(this.src)).collect(Collectors.toSet())));
        }

        if (!blocked.contains(this.dst)) {
            // N.B. SRecv subj is this.dst
            SRecv<DynamicActionKind> a = mf.GTSRecv(this.dst, this.src, this.op, Payload.EMPTY_PAYLOAD, c, n);  // FIXME empty
            res.add(a);
        }
        this.cases.get(this.op).getActs(mf, theta, blocked, c, n).stream()
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
        throw new RuntimeException("Unsupported operation: " + this);
    }

    @Override
    public Set<Op> getCommittingLeft(Role obs, Set<Role> com) {
        throw new RuntimeException("Unsupported operation: " + this);
    }

    @Override
    public Set<Op> getCommittingRight(Role obs, Set<Role> com) {
        throw new RuntimeException("Unsupported operation: " + this);
    }

    @Override
    public Pair<Set<Op>, Map<Integer, Pair<Set<Op>, Set<Op>>>> getLabels() {
        throw new RuntimeException("Shouldn't get here: " + this);
    }

    /* Aux */

    @Override
    public GTGWiggly subs(Map<RecVar, GTGType> subs) {
        LinkedHashMap<Op, GTGType> cases = this.cases.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        x -> x.getValue().subs(subs),
                        (x, y) -> null,
                        LinkedHashMap::new
                ));
        return new GTGWiggly(this.src, this.dst, this.op, cases);
    }

    @Override
    public GTGWiggly unfoldAllOnce() {
        return this;
    }

    @Override
    public Set<Role> getRoles() {
        return Stream.concat(Stream.of(this.dst),
                        this.cases.values().stream().flatMap(x -> x.getRoles().stream()))
                .collect(Collectors.toSet());
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
        ops.add(this.op);  // Not assuming single-pointedness
        return ops;
    }

    @Override
    public Set<RecVar> getRecDecls() {
        return this.cases.values().stream()
                .flatMap(x -> x.getRecDecls().stream()).collect(Collectors.toSet());
    }

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
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTGWiggly)) { return false; }
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
