package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.model.DynamicActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.*;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Discard;
import org.scribble.ext.gt.core.model.local.GTEModelFactory;
import org.scribble.ext.gt.core.model.local.Sigma;
import org.scribble.ext.gt.core.model.local.action.GTEAction;
import org.scribble.ext.gt.core.model.local.action.GTERecv;
import org.scribble.ext.gt.core.model.local.action.GTESend;
import org.scribble.ext.gt.core.type.session.global.GTGInteraction;
import org.scribble.ext.gt.util.Either;
import org.scribble.ext.gt.util.GTUtil;
import org.scribble.ext.gt.util.Quad;
import org.scribble.ext.gt.util.Tree;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

// !!! FIXME naming "interaction" vs. "choice" (in other places)
public class GTLBranch implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final Role src;  // Sender
    public final Map<Op, Payload> pays;  // Pre: Unmodifiable -- keyset subset of cases; values non-null
    public final Map<Op, GTLType> cases;  // Pre: Unmodifiable

    protected GTLBranch(Role src, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTLType> cases) {
        this.src = src;
        this.pays = Collections.unmodifiableMap(pays.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
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
        LinkedHashMap<Op, Payload> pays = new LinkedHashMap<>();
        LinkedHashMap<Op, GTLType> tmp = new LinkedHashMap<>();
        Iterator<Op> it = Stream.of(this.cases.keySet(), cast.cases.keySet()).flatMap(Collection::stream).iterator();
        while (it.hasNext()) {
            Op x = it.next();
            if (this.cases.containsKey(x)) {  // Would be nice if get returned Optional... can map empty directly
                if (cast.cases.containsKey(x)) {
                    // case x in both branches: !!! currently recursively merging but could just simplify to equality
                    Optional<? extends GTLType> opt = this.cases.get(x).merge(cast.cases.get(x));
                    if (!opt.isPresent()) {
                        return Optional.empty();
                    }
                    tmp.put(x, opt.get());
                    if (this.pays.containsKey(x)) {
                        pays.put(x, this.pays.get(x));
                    }
                } else {
                    tmp.put(x, this.cases.get(x));
                    if (this.pays.containsKey(x)) {
                        pays.put(x, this.pays.get(x));
                    }
                }
            } else { //if (cast.cases.keySet().contains(x)) {
                tmp.put(x, cast.cases.get(x));
                if (cast.pays.containsKey(x)) {
                    pays.put(x, cast.pays.get(x));
                }
            }
        }
        return Optional.of(this.fact.branch(this.src, pays, tmp));
    }

    @Override
    public GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                            int c, GTVState s, GTVState end) {
        //GTVState init = new GTVState();
        Set<GTVState> S = new LinkedHashSet<>();
        //S.add(init);
        S.add(s);
        Set<GTVEvent> E = new LinkedHashSet<>();
        Set<GTVAction> A = new LinkedHashSet<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = new LinkedHashMap<>();
        for (Map.Entry<Op, GTLType> x : this.cases.entrySet()) {
            Op op_i = x.getKey();
            GTLType succ_i = x.getValue();
            //Map<Integer, Pair<GTVRecv, GTVState>> stars = com.contains(op_i) ? Map.of() : recvStars;
            Map<Integer, Pair<GTVRecv, GTVState>> stars = new HashMap<>(recvStars);
            for (Map.Entry<Integer, Set<Op>> y : com.entrySet()) {
                if (y.getValue().contains(op_i)) {
                    stars.remove(y.getKey());
                }
            }
            GTVState s_i = new GTVState(c);
            GTEFSM m_i = succ_i.construct(r, com, stars, c, s_i, end);
            S.addAll(m_i.S);
            E.addAll(m_i.E);
            A.addAll(m_i.A);
            GTVRecv e = new GTVRecv(this.src, op_i, this.pays.get(op_i));

            Set<Pair<GTVAction, GTVState>> tmp = delta.computeIfAbsent(new Pair<>(s, e), y -> new LinkedHashSet<>());
            tmp.add(new Pair<>(GTVEpsilon.EPSILON, m_i.init));
            for (Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> y : m_i.delta.entrySet()) {
                Pair<GTVState, GTVEvent> k = y.getKey();
                Set<Pair<GTVAction, GTVState>> tmp2 = delta.computeIfAbsent(k, z -> new LinkedHashSet<>());
                tmp2.addAll(y.getValue());
            }
        }

        GTLMixedChoice.drawExternals(recvStars, s, delta);
        return new GTEFSM(S, s, E, A, delta);
    }


    /* ... */

    @Override
    public GTLBranch subs(RecVar rv, GTLType t) {
        LinkedHashMap<Op, GTLType> cases = this.cases.entrySet().stream()
                                                     .collect(Collectors.toMap(
                                                             Map.Entry::getKey,
                                                             x -> x.getValue().subs(rv, t),
                                                             (x, y) -> null,
                                                             LinkedHashMap::new
                                                     ));
        return this.fact.branch(this.src, new LinkedHashMap<>(this.pays), cases);
    }

    @Override
    public GTLBranch unfoldAllImmediateRecs() {
        return this;
    }

    @Override
    public String toString() {
        return this.src + "&{"
                + this.cases.entrySet().stream()
                            .map(e -> msgToString(e.getKey()) + "." + e.getValue())
                            .collect(Collectors.joining(", "))
                + "}";
    }

    protected String msgToString(Op op) {
        //return op + (!this.pays.containsKey(op) ? "" : "(" + this.pays.get(op) + ")");
        return GTGInteraction.msgToString(op, this.pays.get(op));
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.BRANCH_HASH;
        hash = 31 * hash + this.src.hashCode();
        hash = 31 * hash + this.pays.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLBranch)) { return false; }
        GTLBranch them = (GTLBranch) obj;
        return them.canEquals(this)
                && this.src.equals(them.src)
                && this.pays.equals(them.pays)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLBranch;
    }












    /* ... */

    @Override
    //public LinkedHashSet<EAction<DynamicActionKind>> getActs(
    public LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> getActs(
            GTEModelFactory mf, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        Optional<GTESend<DynamicActionKind>> first = sigma.map.get(this.src)
                                                              .stream().filter(x -> x.c == c && x.n == n).findFirst();
        if (first.isPresent()) {
            GTESend<DynamicActionKind> m = first.get();
            //return Stream.of(m.toDynamicDual(this.src)).collect(Collectors.toCollection(LinkedHashSet::new));
            LinkedHashMap<EAction<DynamicActionKind>, Set<RecVar>> res = GTUtil.mapOf();
            res.put(m.toDynamicDual(this.src), Collections.emptySet());
            return res;
        } else {
            //return GTUtil.setOf();
            return GTUtil.mapOf();
        }
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> step(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {

        if (!(a instanceof GTERecv<?>) || !sigma.map.containsKey(a.peer)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        GTERecv<DynamicActionKind> cast = (GTERecv<DynamicActionKind>) a;
        GTESend<DynamicActionKind> m = cast.toDynamicDual(self);
        if (!sigma.map.get(a.peer).contains(m)) {
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }

        if (!a.peer.equals(this.src) || !this.cases.keySet().contains(a.mid)  // TODO check payload?
                || cast.c != c || cast.n != n) {
            //System.out.println("99999999: " + !a.peer.equals(this.src) + " ,, " + !this.cases.keySet().contains(a.mid) + " ,, " + (cast.c != c) + " .. " + cast.c + " .. " + c + " ,, " + (cast.n != n));
            return Either.left(newStuck(c, n, theta, this, (GTEAction) a));
        }
        boolean[] found = {false};
        List<GTESend<DynamicActionKind>> tmp = sigma.map.get(a.peer).stream().filter(x -> {
            if (!found[0] && x.equals(m)) {
                found[0] = true;
                return false;
            }
            return true;
        }).collect(Collectors.toList());
        Map<Role, List<GTESend<DynamicActionKind>>> map = new HashMap<>(sigma.map);
        map.put(this.src, tmp);
        Sigma sigma1 = new Sigma(map);
        GTLType succ = this.cases.get(a.mid);
        return Either.right(Pair.of(
                Quad.of(succ, sigma1, theta, Tree.of(
                        toStepJudgeString("[Rcv]", c, n, theta, this, sigma,
                                (GTEAction) a, theta, succ, sigma1)
                )),
                GTUtil.mapOf()
        ));
    }

    /* ... */

    @Override
    public LinkedHashSet<EAction<DynamicActionKind>> getWeakActs(
            GTEModelFactory mf, Set<Op> com, Role self, Set<Role> blocked, Sigma sigma, Theta theta, int c, int n) {
        ////return getActs(mf, self, blocked, sigma, theta, c, n);
        return new LinkedHashSet<>(getActs(mf, self, blocked, sigma, theta, c, n).keySet());
    }

    @Override
    public Either<Exception, Pair<Quad<GTLType, Sigma, Theta, Tree<String>>,
            Map<Pair<Integer, Integer>, Discard>>> weakStep(
            Set<Op> com, Role self, EAction<DynamicActionKind> a, Sigma sigma, Theta theta, int c, int n) {
        return step(com, self, a, sigma, theta, c, n);
    }

    /* Aux */

    @Override
    public Map<Integer, Integer> getActive(Theta theta) {
        return this.cases.values().stream()
                         .flatMap(x -> x.getActive(theta).entrySet().stream())
                         .collect(Collectors.toMap(
                                 Map.Entry::getKey,
                                 Map.Entry::getValue,
                                 (x, y) -> x < y ? x : y,
                                 LinkedHashMap::new
                         ));
    }
}
