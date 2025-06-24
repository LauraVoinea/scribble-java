package org.scribble.ext.gt.core.type.session.local;

import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.*;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.*;
import java.util.stream.Collectors;

// HERE extend ANTLR -- copy frontend stuff from scrib-assrt
public class GTLMixedChoice implements GTLType {

    private final GTLTypeFactory fact = GTLTypeFactory.FACTORY;

    public final int c;
    public final GTLType left;
    public final GTLType right;

    protected GTLMixedChoice(
            int c, GTLType left, GTLType right) {
        this.c = c;
        this.left = left;
        this.right = right;
    }


    @Override
    public Optional<? extends GTLType> merge(GTLType t) {
        if (!(t instanceof GTLMixedChoice)) {
            return Optional.empty();
        }
        GTLMixedChoice cast = (GTLMixedChoice) t;
        if (this.c != cast.c || !this.left.equals(cast.left)
                || !this.right.equals(cast.right)) {
            return Optional.empty();
        }
        Optional<? extends GTLType> opt_l = this.left.merge(cast.left);
        Optional<? extends GTLType> opt_r = this.right.merge(cast.right);
        return opt_l.flatMap(x -> opt_r.map(y -> this.fact.mixedChoice(this.c, x, y)));
    }

    @Override
    public GTEFSM construct(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                            int c, GTVState s, GTVState end) {
        return switch (getKind()) {
            case INTERNAL -> constructInternal(r, com, recvStars, c, s, end);
            case EXTERNAL_OI -> constructExternal(r, com, recvStars, c, s, end);
            case EXTERNAL_II -> constructExternal(r, com, recvStars, c, s, end);
        };
    }

    // No consideration of "nested interrupt edges" due to observer immediately committing on both left/right
    protected GTEFSM constructInternal(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                                       int c, GTVState s, GTVState end) {
        GTLBranch left = (GTLBranch) this.left;
        GTLSelect right = (GTLSelect) this.right;

        // !!! right.cases.size() == 1
        Map<Op, GTEFSM> cases_right = right.cases.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> x.getValue().construct(r, com, recvStars, this.c, new GTVState(this.c), end),
                (x, y) -> null,
                LinkedHashMap::new
        ));

        /*Map<Integer, Pair<GTVRecv, GTVState>> leftStars = new LinkedHashMap<>(recvStars);
        Op op = cases_right.keySet().iterator().next();  // !!! right.cases.size() == 1
        leftStars.put(this.c, new Pair<>(new GTVRecv(r, op, right.pays.get(op)), cases_right.get(op).init));*/
        /*if (s.c != GTVState.NON_MIXED_ENTRY) {
            throw new RuntimeException("Shouldn't get here: " + s.c);
        }*/
        GTVState s1 = new GTVState(true, this.c, s.recvars);
        GTEFSM m_left = left.construct(r, com, recvStars, this.c, s1, end);
        GTVState init = m_left.init;
        Set<GTVState> S = new LinkedHashSet<>(m_left.S);
        Set<GTVEvent> E = new LinkedHashSet<>(m_left.E);
        Set<GTVAction> A = new LinkedHashSet<>(m_left.A);
        //Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = new LinkedHashMap<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = m_left.delta.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, x -> new LinkedHashSet<>(x.getValue())));
        // !!! right.cases.size() == 1
        for (Map.Entry<Op, GTEFSM> x : cases_right.entrySet()) {
            Op op_right = x.getKey();
            GTEFSM m_right = x.getValue();
            S.addAll(m_right.S);
            E.addAll(m_right.E);
            A.addAll(m_right.A);

            GTVSendStar a = new GTVSendStar(right.dst, op_right);
            //Set<Pair<GTVAction, GTVState>> tmp = new LinkedHashSet<>();
            //tmp.add(new Pair<>(a, m_right.init));
            // !!! left.cases.size() == 1
            for (Map.Entry<Op, GTLType> y : left.cases.entrySet()) {
                Op op_left = y.getKey();
                GTVRecv e = new GTVRecv(left.src, op_left, left.pays.get(op_left));
                //delta.put(new Pair<>(init, e), tmp);
                Set<Pair<GTVAction, GTVState>> tmp2 = delta.computeIfAbsent(new Pair<>(init, e), z -> new LinkedHashSet<>());
                tmp2.add(new Pair<>(a, m_right.init));
            }

            Set<Pair<GTVAction, GTVState>> tmp3 = new LinkedHashSet<>();
            tmp3.add(new Pair<>(a, m_right.init));
            GTVTau tau = new GTVTau(op_right);
            delta.put(new Pair<>(init, tau), tmp3);

            //delta.putAll(m_right.delta);
            for (Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> y : m_right.delta.entrySet()) {
                Pair<GTVState, GTVEvent> k = y.getKey();
                Set<Pair<GTVAction, GTVState>> tmp2 = delta.computeIfAbsent(k, z -> new LinkedHashSet<>());
                tmp2.addAll(y.getValue());
            }
        }

        drawExternals(recvStars, init, delta);
        return new GTEFSM(S, init, E, A, delta);
    }

    protected static void drawExternals(Map<Integer,
            Pair<GTVRecv, GTVState>> recvStars, GTVState init, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta) {
        for (Pair<GTVRecv, GTVState> x : recvStars.values()) {
            Set<Pair<GTVAction, GTVState>> tmp = delta.computeIfAbsent(new Pair<>(init, x.left), z -> new LinkedHashSet<>());
            tmp.add(new Pair<>(GTVEpsilonStar.EPSILON_STAR, x.right));
        }
    }

    /* com needs to be Map<Role, Map<Integer, Set<Op>>>  // int is c
    ... or calc/ env manually during construction !!! local is easier than global <<<< */

    protected GTEFSM constructExternal(Role r, Map<Integer, Set<Op>> com, Map<Integer, Pair<GTVRecv, GTVState>> recvStars,
                                       int c, GTVState s, GTVState end) {
        GTLType left = this.left;
        GTLBranch right = (GTLBranch) this.right;

        // !!! right.cases.size() == 1
        Map<Op, GTEFSM> cases_right = right.cases.entrySet().stream().collect(Collectors.toMap(
                Map.Entry::getKey,
                x -> x.getValue().construct(r, com, recvStars, this.c, new GTVState(this.c), end),
                (x, y) -> null,
                LinkedHashMap::new
        ));

        Map<Integer, Pair<GTVRecv, GTVState>> leftStars = new LinkedHashMap<>(recvStars);
        Op op = cases_right.keySet().iterator().next();  // !!! right.cases.size() == 1
        leftStars.put(this.c, new Pair<>(new GTVRecv(right.src, op, right.pays.get(op)), cases_right.get(op).init));
        /*if (s.c != GTVState.NON_MIXED_ENTRY) {
            throw new RuntimeException("Shouldn't get here: " + s.c);
        }*/
        GTVState s1 = new GTVState(true, this.c, s.recvars);
        GTEFSM m_left = left.construct(r, com, leftStars, this.c, s1, end);

        GTVState init = m_left.init;
        Set<GTVState> S = new LinkedHashSet<>(m_left.S);
        Set<GTVEvent> E = new LinkedHashSet<>(m_left.E);
        Set<GTVAction> A = new LinkedHashSet<>(m_left.A);
        //Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = new LinkedHashMap<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> delta = m_left.delta.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, x -> new LinkedHashSet<>(x.getValue())));
        // !!! right.cases.size() == 1
        for (Map.Entry<Op, GTEFSM> x : cases_right.entrySet()) {
            Op op_right = x.getKey();
            GTEFSM m_right = x.getValue();
            S.addAll(m_right.S);
            E.addAll(m_right.E);
            A.addAll(m_right.A);

            //delta.putAll(m_right.delta);
            for (Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> y : m_right.delta.entrySet()) {
                Pair<GTVState, GTVEvent> k = y.getKey();
                Set<Pair<GTVAction, GTVState>> tmp2 = delta.computeIfAbsent(k, z -> new LinkedHashSet<>());
                tmp2.addAll(y.getValue());
            }
        }

        drawExternals(recvStars, init, delta);
        return new GTEFSM(S, init, E, A, delta);
    }

    enum MixedKind {
        INTERNAL,
        EXTERNAL_OI,
        EXTERNAL_II,
    }

    protected MixedKind getKind() {
        if (this.right instanceof GTLSelect) {
            if (this.left instanceof GTLBranch) {
                return MixedKind.INTERNAL;
            }
        } else if (this.right instanceof GTLBranch) {
            if (this.left instanceof GTLSelect) {
                return MixedKind.EXTERNAL_OI;
            } else if (this.left instanceof GTLBranch) {
                return MixedKind.EXTERNAL_II;
            }
        }
        // !!! cf. unfolding examples, e.g., (5), (6)
        throw new RuntimeException("EFSM construction not supported by the implementation: " + this);
    }


    /* ... */

    @Override

    public GTLMixedChoice subs(RecVar rv, GTLType t) {
        GTLType left = this.left.subs(rv, t);
        GTLType right = this.right.subs(rv, t);
        return this.fact.mixedChoice(this.c, left, right);
    }

    @Override
    public String toString() {
        return ConsoleColors.toMixedChoiceString(this.left.toString())
                + ConsoleColors.toMixedChoiceString(" " + ConsoleColors.WHITE_TRIANGLE
                + this.c + " " + this.right);
    }


    /* hashCode, equals, canEquals */

    @Override
    public int hashCode() {
        int hash = GTLType.MIXED_CHOICE_HASH;
        hash = 31 * hash + this.c;
        hash = 31 * hash + this.left.hashCode();
        hash = 31 * hash + this.right.hashCode();
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) { return true; }
        if (obj == null || !(obj instanceof GTLMixedChoice)) { return false; }
        GTLMixedChoice them = (GTLMixedChoice) obj;
        return them.canEquals(this)
                && this.c == them.c
                && this.left.equals(them.left)
                && this.right.equals(them.right);
    }

    @Override
    public boolean canEquals(Object o) {
        return o instanceof GTLMixedChoice;
    }

}
