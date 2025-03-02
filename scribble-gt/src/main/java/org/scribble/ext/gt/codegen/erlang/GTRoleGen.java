package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.Op;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.*;
import org.scribble.util.Pair;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class GTRoleGen {

    public String generate(GProtoName proto, Role r, GTEFSM m) {

        List<ErlangFunc> membs = new LinkedList<>();
        for (GTVState s : m.S) {
            switch (GTGenUtil.getStateKind(m, s)) {
                case END -> { }
                case BRANCH -> membs.addAll(generateBranch(m, s));
                case SELECT -> membs.addAll(generateSelect(m, s));
                case INTERNAL_MIXED -> membs.addAll(generateInternalMixed(m, s));
                case EXTERNAL_MIXED_OI -> membs.addAll(generateExternalMixedOI(m, s));
                case EXTERNAL_MIXED_II -> membs.addAll(generateExternalMixedII(m, s));
                case EXTERNAL_MIXED_NOT_ENTRY -> membs.addAll(generateExternalMixedNotEntry(m, s));
            }
        }

        return membs.stream().map(Object::toString).collect(Collectors.joining("\n\n"));
    }

    protected List<ErlangFunc> generateBranch(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateBranchAux(m, s, filt);
    }

    protected List<ErlangFunc> generateBranchAux(
            GTEFSM m, GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
        return edges.entrySet().stream().flatMap(x -> {
            Pair<GTVState, GTVEvent> k = x.getKey();
            GTVRecv e = (GTVRecv) k.right;
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            return vs.stream().map(y -> {
                String name = GTGenUtil.stateToFuncName(s);
                String param_a = GTGenUtil.actionToParam(y.left);  // !!! epsilon?
                List<String> params = List.of("cast", "{" + e.role + "Pid, " + param_a + "}", "Data");
                String body = genNextState(m, y.right);
                return new ErlangFunc(name, params, body);
            });
        }).collect(Collectors.toList());
    }

    protected List<ErlangFunc> generateSelect(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateSelectAux(m, s, filt);
    }

    protected List<ErlangFunc> generateSelectAux(
            GTEFSM m, GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
        List<ErlangFunc> res = new LinkedList<>();
        //res.add(genMakeChoice_s(s));
        res.addAll(edges.entrySet().stream().flatMap(x -> {
            //Pair<GTVState, GTVEvent> k = x.getKey();  // tau_a
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            return vs.stream().map(y -> {
                GTVSend a = (GTVSend) y.left;
                //Role r = (a instanceof GTVSend) ? ((GTVSend) a).role : ((GTVSendStar) a).role;
                String name = GTGenUtil.stateToFuncName(s);
                String param_a = GTGenUtil.actionToParam(a);
                List<String> params = List.of("internal", "{" + param_a + "}", "Data");
                String body = "gen_role:send_" + param_a + "(" + a.role + "Pid, " + param_a + "),\n"
                        + genNextState(m, y.right);
                return new ErlangFunc(name, params, body);
            });
        }).collect(Collectors.toList()));
        return res;
    }

    protected List<ErlangFunc> generateInternalMixed(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);

        String name = GTGenUtil.stateToFuncName(s);
        List<ErlangFunc> res = new LinkedList<>();

        //res.add(genMakeChoice_s(s));

        // !!! TODO missing ?/!* case
        /*Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                filt.entrySet().stream().filter(x ->
                        x.getValue().stream().anyMatch(y -> y.left instanceof GTVSendStar)).collect(
                        Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));*/
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        if (rhs.size() != 1) {
            throw new RuntimeException("Shouldn't get here? " + rhs);
        }
        Set<Pair<GTVAction, GTVState>> sendStars = rhs.values().iterator().next();
        if (sendStars.size() != 1) {
            throw new RuntimeException("Shouldn't get here? " + rhs);
        }

        Pair<GTVAction, GTVState> sendStar = sendStars.iterator().next();
        GTVSendStar a = (GTVSendStar) sendStar.left;
        String param_a = GTGenUtil.actionToParam(a);
        List<String> params = List.of("internal", "{" + param_a + "}", "Data");
        String body = "case make_choice_" + param_a + "(" + a.role + "Pid, Data),\n"
                + genNextState(m, sendStar.right);
        res.add(new ErlangFunc(name, params, body));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x.getKind() == GTVEvent.Kind.EXTERNAL);
        res.addAll(lhs.entrySet().stream().map(x -> {
            Pair<GTVState, GTVEvent> k = x.getKey();
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            GTVRecv e = (GTVRecv) k.right;
            if (vs.size() != 1 && vs.stream().filter(y -> y.left instanceof GTVEpsilon).count() != 1) {
                throw new RuntimeException("Shouldn't get here: " + k + " ,, " + vs);
            }
            Pair<GTVAction, GTVState> succ = vs.iterator().next();
            String a1 = e.op.toString();  // !!! pay?
            List<String> ps = List.of("cast", "{" + e.role + "Pid, " + a1 + ", Data");
            String next = genNextState(m, succ.right);
            String b = "case make_choice_" + a1 + "(Data) of\n"
                    + "1 -> " + next + "\n"
                    + "2 -> gen_role:send_" + a1 + "(" + e.role + "Pid, Data),\n"
                    + next;
            return new ErlangFunc(name, ps, b);
        }).collect(Collectors.toList()));

        return res;
    }

    protected List<ErlangFunc> generateExternalMixedOI(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(m, s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(generateBranchAux(m, s, rhs));

        return res;
    }

    protected List<ErlangFunc> generateExternalMixedII(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(m, s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        res.addAll(generateBranchAux(m, s, rhs));

        return res;
    }

    protected List<ErlangFunc> generateExternalMixedNotEntry(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(m, s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs_tau =  // !!! -- ! |> ?
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(m, s, lhs_tau));

        return res;
    }

    // !!! move to gen_role
    protected ErlangFunc genMakeChoice_s(GTVState s) {
        String name = "make_choice_" + GTGenUtil.stateToFuncName(s);
        List<String> params = List.of("Data");
        String body = "rand:uniform(2)";
        return new ErlangFunc(name, params, body);
    }

    // !!! move to gen_role
    // !!! pay?  -- ! and !*
    protected ErlangFunc genMakeChoice_a(Op op) {
        String name = "make_choice_" + op;
        List<String> params = List.of("Data");
        String body = "rand:uniform(2)";
        return new ErlangFunc(name, params, body);
    }

    protected String genNextState(GTEFSM m, GTVState succ) {
        switch (GTGenUtil.getStateKind(m, succ)) {
            case END:
                return "{stop, normal, Data}";
            case SELECT:
            case INTERNAL_MIXED:  // !!! what if don't want to interrupt (yet)?
            case EXTERNAL_MIXED_OI:
                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt
                        = GTGenUtil.filterEdgesByState(m, succ);
                String s = GTGenUtil.stateToFuncName(succ);
                return
                        "case make_choice_" + s + "(Data) of\n" + filt.keySet().stream().map(x -> {
                            GTVTau tau = (GTVTau) x.right;
                            String a = tau.op.toString();  //actionToParam(tau.op...);   // !!! pay?
                            return a + " -> {next_state, " + s + ", Data, [next_event, internal, {" + a + "}]}";
                        }).collect(Collectors.joining("\n"));
            case BRANCH:
            case EXTERNAL_MIXED_II:
            case EXTERNAL_MIXED_NOT_ENTRY:
                return "{next_state, " + GTGenUtil.stateToFuncName(succ) + ", Data}";
        }
        throw new RuntimeException("Shouldn't get here?");
    }
}
