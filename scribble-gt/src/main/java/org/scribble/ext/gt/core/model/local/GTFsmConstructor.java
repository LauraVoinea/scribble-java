package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.gt.core.type.session.local.*;
import org.scribble.ext.gt.util.ConsoleColors;
import org.scribble.util.Pair;

import java.util.*;

public class GTFsmConstructor {

    public static final GTEModelFactory MF = (GTEModelFactory) GTEModelFactoryImpl.FACTORY.local;

    public GTEState construct(Set<Op> com, GTLType t) {
        GTEState init = peekNewState(t);
        GTEState end = newState();
        return construct(com, new HashMap<>(), end, Optional.empty(), t, init);
    }

    // pend, cf. recvar directly under rec/mixed, recursion/mixed don't directly do peek -- but maybe impossible in core?
    protected GTEState construct(
            Set<Op> com, Map<RecVar, GTEState> recs, GTEState end,
            Optional<Pair<GTEState, EAction<StaticActionKind>>> pend, GTLType t, GTEState s) {
        if (t instanceof GTLBranch cast) {
            return constructBranch(com, recs, end, pend, cast, s);
        } else if (t instanceof GTLSelect cast) {
            return constructSelect(com, recs, end, pend, cast, s);
        } else if (t instanceof GTLRecursion cast) {
            return constructRecursion(com, recs, end, pend, cast, s);
            //} else if (t instanceof GTLRecVar) {
        } else if (t instanceof GTLMixedChoice cast) {
            return constructMixed(com, recs, end, pend, cast, (GTEMixedState) s);
        } else if (t instanceof GTLEnd cast) {
            return constructEnd(end, pend);
        }
        throw new RuntimeException("CHECKME: " + t.getClass());
    }

    protected GTEState constructBranch(
            Set<Op> com, Map<RecVar, GTEState> recs, GTEState end,
            Optional<Pair<GTEState, EAction<StaticActionKind>>> pend, GTLBranch t, GTEState s) {
        prePeekAndEnd(pend, s);
        for (Map.Entry<Op, GTLType> e : t.cases.entrySet()) {
            Op op = e.getKey();
            ERecv<StaticActionKind> a = MF.StaticERecv(t.src, op, t.pays.get(op));
            //GTEState succ = construct(com, recs, e.getValue(), newState());
            //s.addEdge(a, succ);
            peek(com, recs, end, s, a, e.getValue());
        }
        return s;
    }

    private void prePeekAndEnd(Optional<Pair<GTEState, EAction<StaticActionKind>>> pend, GTEState s) {
        if (pend.isPresent()) {
            Pair<GTEState, EAction<StaticActionKind>> p = pend.get();
            p.left.addEdge(p.right, s);
        }
    }

    protected GTEState constructSelect(
            Set<Op> com, Map<RecVar, GTEState> recs, GTEState end,
            Optional<Pair<GTEState, EAction<StaticActionKind>>> pend, GTLSelect t, GTEState s) {
        prePeekAndEnd(pend, s);
        for (Map.Entry<Op, GTLType> e : t.cases.entrySet()) {
            Op op = e.getKey();
            ESend<StaticActionKind> a = MF.StaticESend(t.dst, op, t.pays.get(op));
            //GTEState succ = construct(com, recs, e.getValue(), newState());
            //s.addEdge(a, succ);
            peek(com, recs, end, s, a, e.getValue());
        }
        return s;
    }

    protected GTEState peek(
            Set<Op> com, Map<RecVar, GTEState> recs, GTEState end,
            GTEState prev, EAction<StaticActionKind> a, GTLType t) {
        if (t instanceof GTLRecVar) {
            GTEState succ = recs.get(((GTLRecVar) t).var);
            prev.addEdge(a, succ);
            return succ;
        } else {
            return construct(com, recs, end, Optional.of(new Pair<>(prev, a)), t, peekNewState(t));
        }
    }

    protected GTEState constructRecursion(
            Set<Op> com, Map<RecVar, GTEState> recs, GTEState end,
            Optional<Pair<GTEState, EAction<StaticActionKind>>> pend, GTLRecursion t, GTEState s) {
        HashMap<RecVar, GTEState> recs1 = new HashMap<>(recs);
        recs1.put(t.var, s);
        return construct(com, recs1, end, pend, t.body, s);  // CHECKME can t.body be recvar? (then need peek)
    }

    // Pre: t.right init is not recursive
    protected GTEState constructMixed(
            Set<Op> com, Map<RecVar, GTEState> recs, GTEState end,
            Optional<Pair<GTEState, EAction<StaticActionKind>>> pend, GTLMixedChoice t, GTEMixedState s) {
        //*
        GTEState left = construct(com, recs, end, pend, t.left, s);
        GTEState right = construct(com, recs, end, Optional.empty(), t.right, newState());
        List<EAction<StaticActionKind>> as = right.getActions();
        if (as.size() != 1) {
            throw new RuntimeException("CHECKME " + as);
        }
        EAction<StaticActionKind> fst = as.get(0);
        join(new HashSet<>(), com, left, makeStar(fst), right.getDetSuccessor(fst));  // HERE HERE draw joins as special exception edges -- just add *
        return left;
        /*/ // XXX doesn't draw nested mixed exception edges
        construct(com, recs, end, t.left, s);
        construct(com, recs, end, t.right, s);
        return s;
        //*/
    }

    protected void join(
            Set<Integer> seen, Set<Op> com, GTEState left,
            EAction<StaticActionKind> aRightStar, GTEState rightSucc) {
        if (seen.contains(left.id)) {
            return;
        }
        seen.add(left.id);
        for (EAction<StaticActionKind> a : new ArrayList<>(left.getActions())) {
            if (!com.contains((Op) a.mid)) {
                join(seen, com, left.getDetSuccessor(a), aRightStar, rightSucc);
            }
        }
        left.addEdge(aRightStar, rightSucc);  // Must come after recursive visit above
    }

    protected GTEState constructEnd(
            GTEState end, Optional<Pair<GTEState, EAction<StaticActionKind>>> pend) {
        prePeekAndEnd(pend, end);
        return end;
    }


    /* ... */

    // Pre: t static and not recvar
    protected static GTEState peekNewState(GTLType t) {
        if (t instanceof GTLMixedChoice) {
            return newMixedState();
        } else if (t instanceof GTLRecursion cast) {
            return peekNewState(cast.body);
        } else {
            return newState();
        }
    }

    protected static GTEState newState() {
        return new GTEState(Collections.emptySet());
    }

    protected static GTEMixedState newMixedState() {
        // mark right action instead of |> -- n.b. need to handle nested mixed -- XXX `*` join edges enough?
        return new GTEMixedState(Set.of(new RecVar(Character.toString(ConsoleColors.WHITE_TRIANGLE))));
    }

    protected static EAction<StaticActionKind> makeStar(EAction<StaticActionKind> a) {
        if (a instanceof ESend<StaticActionKind>) {
            return MF.StaticESend(a.peer, new Op("*" + a.mid.toString()), a.payload);  // !!! HACK
        } else if (a instanceof ERecv<StaticActionKind>) {
            return MF.StaticERecv(a.peer, new Op("*" + a.mid.toString()), a.payload);  // !!! HACK
        } else {
            throw new RuntimeException("CHECKME: " + a);
        }
    }


}
