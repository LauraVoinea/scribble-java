package org.scribble.ext.gt.codegen.erlang;

import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.GTVAction;
import org.scribble.ext.gt.core.model.efsm.event.GTVEvent;
import org.scribble.ext.gt.core.model.efsm.event.GTVSendStar;
import org.scribble.util.Pair;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class GTGenUtil {

    public static String actionToParam(GTVAction a) {
        return a.toString();  // XXX epsilon
    }

    public static String stateToFuncName(GTVState s) {
        return "s" + s.id;
    }


    /* ... */

    enum StateKind {
        END,
        BRANCH,  //            &      -- events ?        -- actions eps
        SELECT,  //            (+)    -- events tau      -- actions !
        INTERNAL_MIXED,  //    ? |> ! -- events ? |> tau -- actions eps |> !*  -- !!! XXX all states can have ?/eps*, incl. int_mixed
        EXTERNAL_MIXED_OI,  // ! |> ? -- events tau |> ? -- actions ! |> eps*
        EXTERNAL_MIXED_II,  // ? |> ? -- events ? |> ?   -- actions eps |> eps*
        EXTERNAL_MIXED_NOT_ENTRY  // Can be ? |> ? or ! |> ? -- events/actions same as prev
    }

    public static StateKind getStateKind(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = filterEdgesByState(m, s);
        if (filt.isEmpty()) {
            return StateKind.END;
        }
        Set<Pair<GTVState, GTVEvent>> ks = filt.keySet();
        //if (s.isMixedEntry()) {
        if (s.isEntry) {
            if (ks.stream().allMatch(x -> x.right.getKind() == GTVEvent.Kind.EXTERNAL)) {
                return StateKind.EXTERNAL_MIXED_II;
            } else {
                return filt.values().stream().anyMatch(x -> x.stream().anyMatch(y -> y.left instanceof GTVSendStar))
                       ? StateKind.INTERNAL_MIXED
                       : StateKind.EXTERNAL_MIXED_OI;
            }
        } else {
            return ks.stream().allMatch(x -> x.right.getKind() == GTVEvent.Kind.INTERNAL)
                   ? StateKind.SELECT
                   : ks.stream().allMatch(x -> x.right.getKind() == GTVEvent.Kind.EXTERNAL)
                     ? StateKind.BRANCH
                     : StateKind.EXTERNAL_MIXED_NOT_ENTRY;
        }
    }

    protected static Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filterEdgesByState(
            GTEFSM m, GTVState s) {
        return m.delta.entrySet().stream().filter(x -> x.getKey().left.equals(s))
                      .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));
    }

    protected static Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filterEdgesByEvent(
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt, Predicate<GTVEvent> p) {
        return filt.entrySet().stream().filter(x -> p.test(x.getKey().right)).collect(Collectors.toMap(
                Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));
    }

    protected static Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filterEdgesByAnyAction(
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt, Predicate<GTVAction> p) {
        return filt.entrySet().stream().filter(x -> x.getValue().stream().anyMatch(y -> p.test(y.left)))
                   .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));
    }
}
