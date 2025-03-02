package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.GProtoName;
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
import java.util.stream.Stream;

public class GTGenRoleGen {

    public String generate(GProtoName proto, Role r, GTEFSM m) {

        List<ErlangFunc> membs = new LinkedList<>();
        membs.addAll(generateTop());
        for (GTVState s : m.S) {
            switch (GTGenUtil.getStateKind(m, s)) {
                case END -> { }
                case BRANCH -> membs.addAll(generateBranchAux(m, s));
                case SELECT -> membs.addAll(generateSelect(m, s));
                case INTERNAL_MIXED -> membs.addAll(generateInternalMixed(m, s));
                case EXTERNAL_MIXED_OI -> membs.addAll(generateExternalMixedOI(m, s));
                case EXTERNAL_MIXED_II -> membs.addAll(generateExternalMixedII(m, s));
                case EXTERNAL_MIXED_NOT_ENTRY -> membs.addAll(generateExternalMixedNotEntry(m, s));
            }
        }

        return membs.stream().map(Object::toString).collect(Collectors.joining("\n\n"));
    }

    protected List<ErlangFunc> generateTop() {
        return List.of(
                new ErlangFunc("pid", List.of(), "%TODO"),
                new ErlangFunc("state_data", List.of(), "%TODO"));
    }

    protected List<ErlangFunc> generateBranchAux(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();
        res.addAll(generateBranchAux(s, filt));
        res.add(genGC(s));
        return res;
    }

    protected List<ErlangFunc> generateBranchAux(
            GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt) {
        return filt.entrySet().stream().flatMap(x -> {
            Pair<GTVState, GTVEvent> k = x.getKey();
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            return vs.stream().flatMap(y -> {
                String name = GTGenUtil.stateToFuncName(s);
                GTVRecv e = (GTVRecv) k.right;
                String param_a = GTGenUtil.actionToParam(y.left);
                List<String> params = List.of(
                        "EventType",
                        "{" + e.role + ", " + param_a + ", Counter}",
                        "Data = #state_data{mc_counter_" + s.c + " = MC}");
                String when = "Counter >= MC";
                String body = "CallbackModule = get(callback_module),\n"
                        + "CallbackModule:" + name + "(EventType, {" + e.role + ", " + param_a + "}, Data}";
                return Stream.of(new ErlangFunc(name, params, when, body));
            });
        }).collect(Collectors.toList());
    }

    protected List<ErlangFunc> generateSelect(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateSelectAux(s, filt);
    }

    // Takes both ! and !*
    protected List<ErlangFunc> generateSelectAux(
            GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt) {
        List<ErlangFunc> res = new LinkedList<>();

        res.addAll(filt.entrySet().stream().flatMap(x -> {
            //Pair<GTVState, GTVEvent> k = x.getKey();
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            //GTVTau e = (GTVTau) k.right;
            if (vs.size() != 1 && vs.stream().filter(y -> y.left instanceof GTVSendStar).count() != 1) {  // !!! internal-mix has ? with both eps and !*:w
                throw new RuntimeException("Shouldn't get in here: " + s);
            }
            GTVAction a = vs.iterator().next().left;
            Role r = (a instanceof GTVSend) ? ((GTVSend) a).role : ((GTVSendStar) a).role;  // !!!
            String param_a = GTGenUtil.actionToParam(a);

            String name1 = "send_" + param_a;
            List<String> params1 = List.of(r + "Pid", "Data");
            String body1 = "Counter = Data#state_data.mc_counter_" + s.c + "\n"
                    + "gen_statem:cast(" + r + "Pid, {self{}, " + param_a + ", " + s.c + ", Counter})";
            ErlangFunc send = new ErlangFunc(name1, params1, body1);

            String name2 = GTGenUtil.stateToFuncName(s);
            List<String> params2 = List.of("EventType", "{" + param_a + "}", "Data");
            String body2 = "CallbackModule:" + name2 + "(EventType, {" + param_a + "}, Data}";
            ErlangFunc state = new ErlangFunc(name2, params2, body2);

            return Stream.of(send, state);
        }).collect(Collectors.toList()));

        return res;
    }

    // !!! FIXME can also have (outer) ?/eps* ?
    protected List<ErlangFunc> generateInternalMixed(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVSendStar);  // !!! for ?, a can be either eps or !* (i.e., non-det vs)
        res.addAll(generateSelectAux(s, rhs));

        String name = GTGenUtil.stateToFuncName(s);
        res.addAll(rhs.entrySet().stream().flatMap(x -> {
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            return vs.stream().flatMap(y -> {
                GTVSendStar a = (GTVSendStar) y.left;

                List<String> params = List.of("EventType", "{" + a.role + "");
                String when = "Counter >= MC";
                String body = "CallbackModule = get(callback_module)\n"
                        + "CallbackModule" + name + "(EventType, {" + a.role + "}, Data}";
                return Stream.of(new ErlangFunc(name, params, when, body));
            });
        }).collect(Collectors.toList()));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(lhs.entrySet().stream().map(x -> {
            GTVRecv e = (GTVRecv) x.getKey().right;
            String param_a = e.op.toString();  // !!! pay
            List<String> params = List.of("EventType", "{" + param_a + "}", "Data = #state_data{mc_counter_" + s.c + " = MC}");
            String body = "NewData = Data#State_data{mc_counter_" + s.c + " = MC + 1,\n"
                    + "CallbackModule = get(callback_module),\n"
                    + "CallbackMpodule:" + name + "(EventType, {" + param_a + "}, NewData}";
            return new ErlangFunc(name, params, body);
        }).collect(Collectors.toList()));

        res.add(genGC(s));
        return res;
    }

    protected List<ErlangFunc> generateExternalMixedOI(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVSend);
        res.addAll(genExtMixLHSAux(s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);  // pre: a is eps*
        res.addAll(genExtMixRHSAux(s, rhs));

        res.add(genGC(s));
        return res;
    }

    // cf. generateSelectAux, does counter inc?
    protected List<ErlangFunc> genExtMixLHSAux(GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs) {
        String name = GTGenUtil.stateToFuncName(s);
        return lhs.entrySet().stream().flatMap(x -> {
            Pair<GTVState, GTVEvent> k = x.getKey();
            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
            return vs.stream().flatMap(y -> {
                String param_a = GTGenUtil.actionToParam(y.left);
                List<String> params = List.of("EventType", "{" + param_a + "}", "Data = #state_data{mc_counter_" + s.c + " = MC}");
                String body = "NewData = Data#state_data{mc_counter_" + s.c + " = MC + 1},\n"
                        + "CallbackModule get(callback_module),\n"
                        + "CallbackModule:" + name + "(EventType, {" + param_a + "}, NewData}";
                return Stream.of(new ErlangFunc(name, params, body));
            });
        }).collect(Collectors.toList());
    }

    // cf. generateBranchAux, does counter inc?
    protected List<ErlangFunc> genExtMixRHSAux(GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs) {
        String name = GTGenUtil.stateToFuncName(s);
        return rhs.entrySet().stream().map(x -> {
            Pair<GTVState, GTVEvent> k = x.getKey();
            GTVRecv e = (GTVRecv) k.right;
            String param_a = e.op.toString();  // !!! pay?
            List<String> params = List.of("EventType", "{" + e.role + ", " + param_a + ", Counter}", "Data = #state_data{mc_counter_" + s.c + " = MC}");
            String when = "Clounter >= MC";
            String body = "NewData = Data#state_data{mc_counter_" + s.c + " = MC + 1},\n"
                    + "CallbackModule get(callback_module),\n"
                    + "CallbackModule:" + name + "(EventType, {" + param_a + "}, NewData}";
            return new ErlangFunc(name, params, when, body);
        }).collect(Collectors.toList());
    }

    protected List<ErlangFunc> generateExternalMixedII(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(genExtMixLHSAux(s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        res.addAll(genExtMixRHSAux(s, rhs));

        res.add(genGC(s));
        return res;
    }

    protected List<ErlangFunc> generateExternalMixedNotEntry(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlangFunc> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs_tau =  // !!!
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateBranchAux(s, lhs_tau));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        //res.addAll(genExtMixRHS(s, rhs));  // XXX don't want counter inc
        String name = GTGenUtil.stateToFuncName(s);
        res.addAll(rhs.entrySet().stream().map(x -> {
            Pair<GTVState, GTVEvent> k = x.getKey();
            GTVRecv e = (GTVRecv) k.right;
            String param_a = e.op.toString();  // !!! pay?
            List<String> params = List.of("EventType", "{" + e.role + ", " + param_a + ", Counter}", "Data = #state_data{mc_counter_" + s.c + " = MC}");
            String when = "Clounter >= MC";
            String body = "CallbackModule get(callback_module),\n"
                    + "CallbackModule:" + name + "(EventType, {" + param_a + "}, NewData}";
            return new ErlangFunc(name, params, when, body);
        }).collect(Collectors.toList()));

        res.add(genGC(s));
        return res;
    }


    /* ... */

    protected ErlangFunc genGC(GTVState s) {
        String name = GTGenUtil.stateToFuncName(s);
        List<String> params = List.of("EventType", "{_Pid, _{l}, _Counter}, Data");
        String body = "{keep_state, Data}";
        return new ErlangFunc(name, params, body);
    }
}
