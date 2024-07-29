package org.scribble.ext.gt.core.model.local;

import org.scribble.core.model.StaticActionKind;
import org.scribble.core.model.endpoint.actions.EAction;
import org.scribble.core.model.endpoint.actions.ERecv;
import org.scribble.core.model.endpoint.actions.ESend;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.RecVar;
import org.scribble.ext.gt.core.type.session.local.*;

import java.util.*;

public class GTFsmConstructor {

    public static final GTEModelFactory mf = (GTEModelFactory) GTEModelFactoryImpl.FACTORY.local;

    public GTEState construct(Set<Op> com, GTLType t) {
        System.out.println("aaaa: " + com);
        return construct(com, new HashMap<>(), newState(), t, newState());
    }

    protected GTEState newState() {
        return new GTEState(Collections.emptySet());
    }

    // HERE HERE Optional<Triple<GTEState, EAction<StaticActionKind>, GTEState> pending
    protected GTEState construct(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTLType t, GTEState s) {
        if (t instanceof GTLBranch) {
            return constructBranch(com, recs, end, (GTLBranch) t, s);
        } else if (t instanceof GTLSelect) {
            return constructSelect(com, recs, end, (GTLSelect) t, s);
        } else if (t instanceof GTLRecursion) {
            return constructRecursion(com, recs, end, (GTLRecursion) t, s);
            //} else if (t instanceof GTLRecVar) {
        } else if (t instanceof GTLMixedChoice) {
            return constructMixed(com, recs, end, (GTLMixedChoice) t, s);
        } else if (t instanceof GTLEnd) {
            return end;  //constructEnd(com, recs, end, (GTLRecursion) t, s);
        }
        throw new RuntimeException("CHECKME: " + t.getClass());
    }

    protected GTEState constructBranch(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTLBranch t, GTEState s) {
        for (Map.Entry<Op, GTLType> e : t.cases.entrySet()) {
            Op op = e.getKey();
            ERecv<StaticActionKind> a = mf.StaticERecv(t.src, op, t.pays.get(op));
            //GTEState succ = construct(com, recs, e.getValue(), newState());
            //s.addEdge(a, succ);
            patch(com, recs, end, s, a, e.getValue());
        }
        return s;
    }

    protected GTEState constructSelect(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTLSelect t, GTEState s) {
        for (Map.Entry<Op, GTLType> e : t.cases.entrySet()) {
            Op op = e.getKey();
            ESend<StaticActionKind> a = mf.StaticESend(t.dst, op, t.pays.get(op));
            //GTEState succ = construct(com, recs, e.getValue(), newState());
            //s.addEdge(a, succ);
            patch(com, recs, end, s, a, e.getValue());
        }
        return s;
    }

    protected GTEState patch(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTEState prev, EAction<StaticActionKind> a, GTLType t) {
        GTEState succ;
        succ = (t instanceof GTLRecVar) ? recs.get(((GTLRecVar) t).var) : construct(com, recs, end, t, newState());
        prev.addEdge(a, succ);
        return succ;
    }

    protected GTEState constructRecursion(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTLRecursion t, GTEState s) {
        HashMap<RecVar, GTEState> recs1 = new HashMap<>(recs);
        recs1.put(t.var, s);
        return construct(com, recs1, end, t.body, s);  // CHECKME can t.body be recvar? (then need patch)
    }

    // Pre: t.right init is not recursive
    protected GTEState constructMixed(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTLMixedChoice t, GTEState s) {
        GTEState left = construct(com, recs, end, t.left, newState());
        GTEState right = construct(com, recs, end, t.right, newState());
        List<EAction<StaticActionKind>> as = right.getActions();
        if (as.size() != 1) {
            throw new RuntimeException("CHECKME " + as);
        }
        EAction<StaticActionKind> fst = as.get(0);
        join(new HashSet<>(), com, left, fst, right.getDetSuccessor(fst));
        return left;
    }

    protected void join(Set<Integer> seen, Set<Op> com, GTEState left, EAction<StaticActionKind> aRight, GTEState rightSucc) {
        if (seen.contains(left.id)) {
            return;
        }
        seen.add(left.id);
        left.addEdge(aRight, rightSucc);
        for (EAction<StaticActionKind> a : new ArrayList<>(left.getActions())) {
            if (!com.contains((Op) a.mid)) {
                join(seen, com, left.getDetSuccessor(a), aRight, rightSucc);
            }
        }
    }

    /*protected GTEState constructEnd(Set<Op> com, Map<RecVar, GTEState> recs, GTEState end, GTLRecursion t, GTEState s) {
        return end;
    }*/


}
