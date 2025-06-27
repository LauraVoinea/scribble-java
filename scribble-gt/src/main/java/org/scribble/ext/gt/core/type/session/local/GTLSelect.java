package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.*;
import org.scribble.ext.gt.core.type.session.global.GTGInteraction;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

public class GTLSelect implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final Role dst;
    public final Map<Op, Payload> pays;  // Pre: Unmodifiable -- keyset subset of cases; values non-null
    public final Map<Op, GTLType> cases;  // Pre: Unmodifiable

    protected GTLSelect(Role dst, LinkedHashMap<Op, Payload> pays, LinkedHashMap<Op, GTLType> cases) {
        this.dst = dst;
        this.pays = Collections.unmodifiableMap(pays.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
        this.cases = Collections.unmodifiableMap(cases.entrySet().stream().collect(
                Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue,
                        (x, y) -> x, LinkedHashMap::new)));
    }


    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        return this.equals(t) ? Optional.of(this) : Optional.empty();
    }

    @Override
    public GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                            int c, GTVState s, GTVState end) {
        Set<GTVState> S = new LinkedHashSet<>();
        //S.add(init);
        S.add(s);
        Set<GTVEvent> E = new LinkedHashSet<>();
        Set<GTVAction> A = new LinkedHashSet<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = new LinkedHashMap<>();
        for (Map.Entry<Op, GTLType> x : this.cases.entrySet()) {
            Op op_i = x.getKey();
            GTLType succ_i = x.getValue();
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
            GTVTau e = new GTVTau(op_i);
            GTVSend a = new GTVSend(this.dst, op_i, this.pays.get(op_i));

            Set<Pair<GTVAction, GTVState>> tmp = delta.computeIfAbsent(new Pair<>(s, e), y -> new LinkedHashSet<>());
            tmp.add(new Pair<>(a, m_i.init));
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
    public GTLSelect subs(RecVar rv, GTLType t) {
        LinkedHashMap<Op, GTLType> cases =
                this.cases.entrySet().stream()
                          .collect(Collectors.toMap(
                                  Map.Entry::getKey,
                                  x -> x.getValue().subs(rv, t),
                                  (x, y) -> null,
                                  LinkedHashMap::new
                          ));
        return this.fact.select(this.dst, new LinkedHashMap<>(this.pays), cases);
    }

    @Override
    public String toString() {
        return this.dst + (ConsoleColors.OLPLUS + "{")
                + this.cases.entrySet().stream()
                            .map(e -> msgToString(e.getKey()) + "." + e.getValue())
                            .collect(Collectors.joining(", "))
                + "}";
    }

    protected String msgToString(Op op) {
        return GTGInteraction.msgToString(op, this.pays.get(op));
    }

    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.SELECT_HASH;
        hash = 31 * hash + this.dst.hashCode();
        hash = 31 * hash + this.pays.hashCode();
        hash = 31 * hash + this.cases.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLSelect)) { return false; }
        GTLSelect them = (GTLSelect) obj;
        return them.canEquals(this)
                && this.dst.equals(them.dst)
                && this.pays.equals(them.pays)
                && this.cases.equals(them.cases);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLSelect;
    }










}
