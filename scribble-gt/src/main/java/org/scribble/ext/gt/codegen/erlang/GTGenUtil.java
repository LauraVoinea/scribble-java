package org.scribble.ext.gt.codegen.erlang;

import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.*;
import org.scribble.util.Pair;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GTGenUtil {


    public static String stateToFuncName(GTVState s) {
        return "s" + s.id;
    }

    public static String eventToParam(GTVEvent e) {
        if (e instanceof GTVRecv cast) {
            return cast.op.toString();  // !!! pay?
        } else if (e instanceof GTVTau cast) {
            return cast.op.toString();  // !!! pay?
        } else {
            throw new RuntimeException("Shouldn't get here: ");
        }
    }

    public static String sendToParam(GTVSend a) {
        return a.op.toString();  // !!! pay?
    }

    public static String sendToParam(GTVSendStar a) {
        return a.op.toString();  // !!! pay?
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

    // Return the number of mixed choices in the GTEFSM m
    public static int getNumMixedChoices(GTEFSM m) {
        return (int) m.S.stream().filter(x -> x.isEntry).count();
    }

    public static Set<GTVEvent> getEvents(GTEFSM m) {
        //get all events from the GTEFSM m
        return m.delta.entrySet().stream()
                .flatMap(x -> x.getKey().right instanceof GTVEvent ?
                        Stream.of((GTVEvent) x.getKey().right) : Stream.empty())
                .collect(Collectors.toSet());

    }

    //get all events to be received in the GTEFSM m starting from state s
    //skipping the events from state s directly; i.e. events to be received from the next state onwards
    public static Set<GTVRecv> getRecvEvents(GTEFSM m, GTVState s) {
        // skipping the events from state s directly; i.e., events to be received from the next state onwards
        Set<GTVRecv> recvs = new HashSet<>();
        Set<GTVState> visited = new HashSet<>();
        Queue<GTVState> queue = new LinkedList<>();
        visited.add(s);
        queue.add(s);
        while (!queue.isEmpty()) {
            GTVState cur = queue.poll();
            // collect receive events for states other than the starting state
            if (!cur.equals(s)) {
                filterEdgesByState(m, cur).keySet().stream()
                    .filter(k -> k.right instanceof GTVRecv)
                    .map(k -> (GTVRecv) k.right)
                    .forEach(recvs::add);
            }
            // enqueue successors
            filterEdgesByState(m, cur).values().stream()
                .flatMap(Set::stream)
                .map(pair -> pair.right)
                .filter(next -> !visited.contains(next))
                .forEach(next -> {
                    visited.add(next);
                    queue.add(next);
                });
        }
        return recvs;
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
