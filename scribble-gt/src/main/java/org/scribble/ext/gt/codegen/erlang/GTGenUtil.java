package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.Op;
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
    // Returns true if the FSM contains any mixed choices
    public static boolean hasMixedChoices(GTEFSM m) {
        return getNumMixedChoices(m) > 0;
    }
    // Checks whether state s is part of a mixed-choice path in m
    public static boolean isOnMixedChoicePath(GTEFSM m, GTVState s) {
        if (!hasMixedChoices(m)) {
            return false;
        }
        // A state lies on a mixed-choice path if it's reachable from any mixed-entry state
        return m.S.stream()
            .filter(entry -> entry.isEntry)
            .anyMatch(entry -> reachable(entry, s, m));
    }

    public static Set<GTVEvent> getEvents(GTEFSM m) {
        //get all events from the GTEFSM m
        return m.delta.entrySet().stream()
                .flatMap(x -> x.getKey().right instanceof GTVEvent ?
                        Stream.of((GTVEvent) x.getKey().right) : Stream.empty())
                .collect(Collectors.toSet());

    }

//    gcEvents for state s:
//            1. State s is not mixed and not a child of a mixed choice: gcEvents == null
//            2. State s is mixed and not a child of MC: gcEvents == all receive events from both sides of the MC
//    3. State s is a child of MC: gcEvents == all receive events from the other side of the parent MC.
//    4. State s is a child of MC and an MC: gcEvents == all receive events from the other side of the parent MC ++ all all receive events from all branches of state s
// filter out Map<Integer, Set<Op>> explicitCommiting
    public static Set<GTVEvent> getGcEvents(GTEFSM m, GTVState s, Map<Integer, Set<Op>> explicitCommiting) {
        StateKind kind = getStateKind(m, s);
        System.err.println("=1=> " + s + " <><> "   + kind + " <><> isEntry " + s.isEntry
                + " <><> c " + s.c + " <><> numMixedChoices " + getNumMixedChoices(m)
        + " <><> recvars " + s.recvars);
        if (kind == StateKind.END) {
            return Collections.emptySet();
        }
        boolean isEntry = s.isEntry;
//        boolean isChild = s.c != getNumMixedChoices(m) && !s.isEntry;
        // 1. non-mixed and not child of MC: nothing to gc
        if (!isEntry && s.c == GTVState.TOP_SCOPE) {
            if(s.recvars.isEmpty()) {
                System.err.println("=1=> " + s + " is not mixed and not a child of MC: no gc events");
                return Collections.emptySet();
            }
        }
        System.err.println("=3=> " + s + " <><> "   + kind + " <><> isEntry " + isEntry
                + " <><> c " + s.c + " <><> numMixedChoices " + getNumMixedChoices(m) + "\n\n");
        // collect all receive events reachable from s (both direct transitions and downstream)
        Set<GTVRecv> branchRecvs = new HashSet<>();
        // direct receive events at s
        filterEdgesByState(m, s).keySet().stream()
            .filter(k -> k.right instanceof GTVRecv)
            .map(k -> (GTVRecv) k.right)
            .forEach(recv -> {
                branchRecvs.add(recv);
            });
        // receive events from downstream states (with state context)
        getRecvEvents(m, s).forEach(pair -> {
            GTVRecv recv = pair.left;
            if (pair.right > 0) {
                branchRecvs.add(recv);
            }
        });
        // 2. mixed entry and not child of MC
        if (isEntry && s.c == getNumMixedChoices(m)) {
            return new HashSet<>(branchRecvs);
        }
        // 3. for nested MC children: collect receives from all ancestor mixed-choice entries
        List<GTVState> ancestors = m.S.stream()
                .filter(x -> x.isEntry && x.c >= s.c)
                .collect(Collectors.toList());

        // collect receive events from the other side of each ancestor MC
        // for each ancestor MC, we collect the receive events from the other side of that MC
        Set<GTVRecv> parentRecvs = getOtherBranchRecvs(m, s);
        // filter out explicitly committing operations
        Set<Op> expOps = explicitCommiting.values().stream().flatMap(Set::stream).collect(Collectors.toSet());
        branchRecvs.removeIf(recv -> expOps.contains(recv.op));
        parentRecvs.removeIf(recv -> expOps.contains(recv.op));
        // 4. child of MC and an MC
        Set<GTVEvent> result = new HashSet<>(parentRecvs);
        result.addAll(branchRecvs);
        return result;
    }

    //get all events to be received in the GTEFSM m starting from state s
    //skipping the events from state s directly; i.e. events to be received from the next state onwards
    public static Set<Pair<GTVRecv, Integer>> getRecvEvents(GTEFSM m, GTVState s) {
        // skipping events from s itself; collect events and their state context 'c'
        Set<Pair<GTVRecv, Integer>> recvs = new HashSet<>();
        Set<GTVState> visited = new HashSet<>();
        Queue<GTVState> queue = new LinkedList<>();
        visited.add(s);
        queue.add(s);
        // collect receive events in starting state to filter out later
        Set<GTVRecv> startRecvs = filterEdgesByState(m, s).keySet().stream()
                .filter(k -> k.right instanceof GTVRecv)
                .map(k -> (GTVRecv) k.right)
                .collect(Collectors.toSet());
        while (!queue.isEmpty()) {
            GTVState cur = queue.poll();
            // collect receive events for states other than the starting state
            if (!cur.equals(s)) {
                filterEdgesByState(m, cur).keySet().stream()
                    .filter(k -> k.right instanceof GTVRecv)
                    .map(k -> (GTVRecv) k.right)
                    .filter(e -> !startRecvs.contains(e))
                    .forEach(e -> recvs.add(new Pair<>(e, cur.c)));
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

    /**
     * Check if target state 'to' is reachable from 'from' in EFSM m.
     */
    public static boolean reachable(GTVState from, GTVState to, GTEFSM m) {
        Set<GTVState> visited = new HashSet<>();
        Queue<GTVState> queue = new LinkedList<>();
        visited.add(from);
        queue.add(from);
        while (!queue.isEmpty()) {
            GTVState cur = queue.poll();
            if (cur.equals(to)) {
                return true;
            }
            // explore successors
            for (var entry : filterEdgesByState(m, cur).values()) {
                for (var pair : entry) {
                    GTVState nxt = pair.right;
                    if (!visited.contains(nxt)) {
                        visited.add(nxt);
                        queue.add(nxt);
                    }
                }
            }
        }
        return false;
    }

    /**
     * Collect all GTVRecv events (direct and downstream) reachable from start state.
     */
    private static Set<GTVRecv> collectRecvsFromState(GTEFSM m, GTVState start) {
        Set<GTVRecv> recvs = new HashSet<>();
        // direct receives at start
        filterEdgesByState(m, start).keySet().stream()
            .filter(k -> k.right instanceof GTVRecv)
            .map(k -> (GTVRecv) k.right)
            .forEach(recvs::add);
        // downstream receives
        getRecvEvents(m, start).stream().map(p -> p.left).forEach(recvs::add);
        return recvs;
    }

    /**
     * Given a state s on one branch of a mixed-choice entry, return the GTVRecv events on the other branch.
     */
    public static Set<GTVRecv> getOtherBranchRecvs(GTEFSM m, GTVState s) {
        // collect recvs from other branches of all ancestor mixed-choice entries
        List<GTVState> mcs = m.S.stream()
            .filter(x -> x.isEntry && x.c >= s.c)
            .collect(Collectors.toList());
        Set<GTVRecv> allRecvs = new HashSet<>();
        for (GTVState mc : mcs) {
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges = filterEdgesByState(m, mc);
            Optional<Pair<GTVState, GTVEvent>> optMyEntry = edges.entrySet().stream()
                .filter(e -> e.getValue().stream().anyMatch(pair -> reachable(pair.right, s, m)))
                .map(Map.Entry::getKey)
                .findFirst();
            if (optMyEntry.isEmpty()) {
                continue; // no branch reaches s in this MC
            }
            Pair<GTVState, GTVEvent> myEntry = optMyEntry.get();
            allRecvs.addAll(
                edges.entrySet().stream()
                    .filter(e -> !e.getKey().equals(myEntry))
                    .flatMap(e -> e.getValue().stream())
                    .flatMap(pair -> collectRecvsFromState(m, pair.right).stream())
                    .collect(Collectors.toSet())
            );
        }
        return allRecvs;
    }
}
