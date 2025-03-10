package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.*;
import org.scribble.ext.gt.core.model.local.GTLConfig;
import org.scribble.util.Pair;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class GTGenericBehaviour {
    private static final String OUTPUT_DIR = "./test";
    private static final String ERL_EXTENSION = ".erl";

    /** Generate and write the Erlang code for the generic behaviour module to a file. */
    public void generateCode(GProtoName protocolName, GTLConfig r, GTEFSM efsm) throws IOException {
        Role role = r.self;
        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName.toString());
        Files.createDirectories(outputDirectory);
        String moduleName = "gen_" + role.toString().toLowerCase();
        String callbackModuleName = role.toString().toLowerCase();

        Path filePath = outputDirectory.resolve(moduleName + ERL_EXTENSION);
        FileWriter writer = new FileWriter(filePath);
        // Module declaration and behaviour
        writer.writeLine("-module(" + moduleName + ").");
        writer.writeLine("-behaviour(gen_statem).");
        writer.writeLine("");

        // Export functions: init/1, callback_mode/0, code_change/4, terminate/3, and state functions
        StringBuilder exports = new StringBuilder();
        exports.append("[init/1, callback_mode/0, code_change/4, terminate/3");
        //TODO: exclude terminal state
        for (GTVState state : efsm.S) {
            if (!GTGenUtil.getStateKind(efsm, state).equals(GTGenUtil.StateKind.END)) {
                String stateName = GTGenUtil.stateToFuncName(state);
                exports.append(", ").append(stateName).append("/3");
            }
        }
        exports.append("]");
        writer.writeLine("-export(" + exports.toString() + ").");
        writer.writeLine("");

        //TODO: actual counter name
        // Define state data record (with counter and user_state fields)
//        writer.writeLine("-record(state_data, {counter = 0, user_state}).");
        writer.writeLine(generateStateDataRecord(efsm, r.sigma.map.keySet()));
        writer.writeLine("");

        // Generate init/1 function
        ErlFun initFun = createInitFunction(callbackModuleName, efsm.init);
        initFun.write(writer);
        writer.writeLine("");

        // Generate callback_mode/0 function
        ErlFun cbModeFun = createCallbackModeFunction();
        cbModeFun.write(writer);
        writer.writeLine("");

        // Generate state functions for each state in the EFSM.
        for (GTVState s : efsm.S) {
            writer.writeLine("%% State " + GTGenUtil.stateToFuncName(s) + " (" + GTGenUtil.getStateKind(efsm, s) + ")");
            switch (GTGenUtil.getStateKind(efsm, s)) {
                case END:
                    break;
                case BRANCH:
                    for (ErlFun f : generateBranchAux(efsm, s)) {
                        f.write(writer);
                        writer.writeLine("");
                    }
                    break;
                case SELECT:
                    for (ErlFun f : generateSelect(efsm, s)) {
                        f.write(writer);
                        writer.writeLine("");
                    }
                    break;
                case INTERNAL_MIXED:
                    for (ErlFun f : generateInternalMixed(efsm, s)) {
                        f.write(writer);
                        writer.writeLine("");
                    }
                    break;
                case EXTERNAL_MIXED_OI:
                    for (ErlFun f : generateExternalMixedOI(efsm, s)) {
                        f.write(writer);
                        writer.writeLine("");
                    }
                    break;
                case EXTERNAL_MIXED_II:
                    for (ErlFun f : generateExternalMixedII(efsm, s)) {
                        f.write(writer);
                        writer.writeLine("");
                    }
                    break;
                case EXTERNAL_MIXED_NOT_ENTRY:
                    for (ErlFun f : generateExternalMixedNotEntry(efsm, s)) {
                        f.write(writer);
                        writer.writeLine("");
                    }
                    break;
            }
        }

        // Generate code_change/4 function for hot code upgrades
        ErlFun codeChangeFun = createCodeChangeFunction();
        codeChangeFun.write(writer);
        writer.writeLine("");

        // Generate terminate/3 function for cleanup
        ErlFun terminateFun = createTerminateFunction();
        terminateFun.write(writer);
        writer.writeLine("");

        writer.close();
    }

    private String generateStateDataRecord(GTEFSM efsm, Set<Role> roles) {
        // Generate counter fields for every state in the EFSM.
        // (Assumes that each state gets a counter field named "mc_counter_<state.id>")
        Set<String> counterFields = efsm.S.stream()
                .filter(s -> s.c > 0)
                .map(s -> "mc_counter_" + s.c + " = 0")
                .collect(Collectors.toCollection(LinkedHashSet::new));

        // Create pid fields for each role.
        Set<String> rolePidFields = roles.stream()
                .map(role -> role.toString().toLowerCase() + "_pid")
                .collect(Collectors.toCollection(LinkedHashSet::new));

        List<String> allFields = new ArrayList<>();
        allFields.addAll(counterFields);
        allFields.addAll(rolePidFields);
        return "-record(state_data, {" + String.join(", ", allFields) + "}).";
    }


    /** Build the init/1 function, which initializes the gen_statem. */
    private ErlFun createInitFunction(String callbackModuleName, GTVState initState) {
        ErlVar argVar = new ErlVar("Arg");
        // Call the callback module's init(Arg) to get initial user state.
        ErlVar userStateVar = new ErlVar("UserState");
        ErlCall callbackInitCall = new ErlCall(callbackModuleName, "init", Arrays.asList(argVar));
        ErlMatch assignUserState = new ErlMatch(userStateVar, callbackInitCall);
        // Create initial StateData = #state{counter=0, user_state=UserState}.
        ErlVar stateDataVar = new ErlVar("StateData");
        ErlRecordUpdate initStateRecord = new ErlRecordUpdate(null, "state_data");
        initStateRecord.addField("counter", new ErlAtom("0"));
        initStateRecord.addField("user_state", userStateVar);
        ErlMatch assignStateData = new ErlMatch(stateDataVar, initStateRecord);
        // Return {ok, s<initState.id>, StateData}.
        String stateName = GTGenUtil.stateToFuncName(initState);
        ErlTuple okTuple = new ErlTuple(Arrays.asList(new ErlAtom("ok"), new ErlAtom(stateName), stateDataVar));
        // Compose the function body sequence.
        ErlSeq initBody = new ErlSeq();
        initBody.addExpression(assignUserState);
        initBody.addExpression(assignStateData);
        initBody.addExpression(okTuple);
        // Define init/1 clause.
        ErlFun initFun = new ErlFun("init");
        initFun.addClause(Arrays.asList(argVar), null, initBody);
        return initFun;
    }

    /** Build the callback_mode/0 function (returns 'state_functions'). */
    private ErlFun createCallbackModeFunction() {
        ErlFun cbModeFun = new ErlFun("callback_mode");
        cbModeFun.addClause(Collections.emptyList(), null, new ErlAtom("state_functions"));
        return cbModeFun;
    }

    /** Build a state function for a given state. */
    private ErlFun createStateFunction(GTVState state, String callbackModuleName, GTEFSM efsm) {
        String stateName = "s" + state.id;
        ErlFun stateFun = new ErlFun(stateName);

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(efsm, state);

        // For each outgoing transition of the state, build two clauses (fresh and stale)
        filt.entrySet().stream().forEach(entry -> {
            GTVEvent event = entry.getKey().right;
            String eventName = GTGenUtil.eventToParam(event);
            entry.getValue().stream().forEach(pair -> {
                // Build head pattern: [ "info", {EventName, C, Msg}, StateData ]
                ErlTerm head1 = new ErlAtom("info");
                ErlVar cVar = new ErlVar("C");
                ErlVar msgVar = new ErlVar("Msg");
                ErlTuple eventPattern = new ErlTuple(Arrays.asList(new ErlAtom(eventName), cVar, msgVar));
                ErlVar stateDataVar = new ErlVar("StateData");
                List<ErlTerm> headArgs = Arrays.asList(head1, eventPattern, stateDataVar);

                // Guard: C >= StateData#state.counter
                ErlRecordAccess currentCount = new ErlRecordAccess(stateDataVar, "state", "counter");
                ErlCall compareGuard = new ErlCall(new ErlOp(">="), Arrays.asList(cVar, currentCount));
                ErlGuard freshGuard = new ErlGuard(compareGuard);

                // Fresh event body:
                // 1. NewUserState = CallbackModule:handle_EventName(Msg, StateData#state.user_state)
                ErlVar newUserStateVar = new ErlVar("NewUserState");
                ErlRecordAccess userStateAccess = new ErlRecordAccess(stateDataVar, "state", "user_state");
                ErlCall callbackCall = new ErlCall(callbackModuleName, "handle_" + eventName,
                        Arrays.asList(msgVar, userStateAccess));
                ErlMatch assignNewUserState = new ErlMatch(newUserStateVar, callbackCall);

                // 2. NewCounter = C + 1
                ErlVar newCounterVar = new ErlVar("NewCounter");
                ErlCall incrementCounter = new ErlCall("+", Arrays.asList(cVar, new ErlAtom("1")));
                ErlMatch assignNewCounter = new ErlMatch(newCounterVar, incrementCounter);

                // 3. NewStateData = StateData#state{counter = NewCounter, user_state = NewUserState}
                ErlVar newStateDataVar = new ErlVar("NewStateData");
                ErlRecordUpdate updatedStateRec = new ErlRecordUpdate(stateDataVar, "state");
                updatedStateRec.addField("counter", newCounterVar);
                updatedStateRec.addField("user_state", newUserStateVar);
                ErlMatch assignNewStateData = new ErlMatch(newStateDataVar, updatedStateRec);

                // 4. Build result tuple based on target state:
                //    - If targetState is null or "stop": {stop, normal, NewStateData}
                //    - Else if targetState equals stateName: {keep_state, NewStateData}
                //    - Otherwise: {next_state, targetState, NewStateData}
                ErlTerm resultTuple;
                String target = pair.right.toString();
                if (target == null || target.equals("stop")) {
                    resultTuple = new ErlTuple(Arrays.asList(new ErlAtom("stop"), new ErlAtom("normal"), newStateDataVar));
                } else if (target.equals(stateName)) {
                    resultTuple = new ErlTuple(Arrays.asList(new ErlAtom("keep_state"), newStateDataVar));
                } else {
                    resultTuple = new ErlTuple(Arrays.asList(new ErlAtom("next_state"), new ErlAtom(target), newStateDataVar));
                }

                // Build the sequence of actions for a fresh event.
                ErlSeq freshBody = new ErlSeq();
                freshBody.addExpression(assignNewUserState);
                freshBody.addExpression(assignNewCounter);
                freshBody.addExpression(assignNewStateData);
                freshBody.addExpression(resultTuple);

                // Add the fresh clause with guard.
                stateFun.addClause(headArgs, freshGuard, freshBody);

                // Build stale clause: same head, no guard, body returns {keep_state, StateData}
                ErlSeq staleBody = new ErlSeq();
                staleBody.addExpression(new ErlTuple(Arrays.asList(new ErlAtom("keep_state"), stateDataVar)));
                stateFun.addClause(headArgs, null, staleBody);
            });
        });

        // (Optional catch-all clause can be added here if desired)
        return stateFun;
    }

    /** Build code_change/4 function for handling code upgrades (no state change). */
    private ErlFun createCodeChangeFunction() {
        ErlFun codeChangeFun = new ErlFun("code_change");
        ErlVar vsnVar = new ErlVar("_Vsn");
        ErlVar stateNameVar = new ErlVar("_StateName");
        ErlVar stateDataVar = new ErlVar("StateData");
        ErlVar extraVar = new ErlVar("_Extra");
        ErlTuple result = new ErlTuple(Arrays.asList(new ErlAtom("ok"), stateDataVar));
        codeChangeFun.addClause(Arrays.asList(vsnVar, stateNameVar, stateDataVar, extraVar), null, result);
        return codeChangeFun;
    }

    /** Build terminate/3 function for cleanup when the state machine stops. */
    private ErlFun createTerminateFunction() {
        ErlFun termFun = new ErlFun("terminate");
        ErlVar reasonVar = new ErlVar("_Reason");
        ErlVar stateVar = new ErlVar("_State");
        ErlVar stateDataVar = new ErlVar("_StateData");
        termFun.addClause(Arrays.asList(reasonVar, stateVar, stateDataVar), null, new ErlAtom("ok"));
        return termFun;
    }

    protected List<ErlFun> generateBranchAux(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        res.addAll(generateBranchAux(s, filt));
        res.add(genGC(s));
        return res;
    }

    protected List<ErlFun> generateBranchAux(
            GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt) {
        return filt.entrySet().stream().flatMap(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            GTVRecv e = (GTVRecv) key.right;
            String funcName = GTGenUtil.stateToFuncName(s);
            String paramA = GTGenUtil.eventToParam(e);
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            Map<String, ErlTerm> fields = new LinkedHashMap<>();
            //TODO: add PIDs
            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern pattern = new ErlRecordPattern("state_data", fields);
            return actions.stream().map(pair -> {
                List<ErlTerm> headArgs = Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), new ErlAtom(paramA), new ErlVar("Counter"))),
                        new ErlMatch(pattern, new ErlVar("Data"))
                );

                ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp(">="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
                ErlSeq bodySeq = new ErlSeq();
                bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                        new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
                bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                        Arrays.asList(
                                new ErlVar("EventType"),
                                new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), new ErlAtom(paramA))),
                                new ErlVar("Data")
                        )));
                ErlFun clauseFun = new ErlFun(funcName);
                clauseFun.addClause(headArgs, guard, bodySeq);
                return clauseFun;
            });
        }).collect(Collectors.toList());
    }

    protected List<ErlFun> generateSelect(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        return generateSelectAux(s, filt);
    }

    // Takes both ! and !*
    protected List<ErlFun> generateSelectAux(
            GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt) {
        return filt.entrySet().stream().flatMap(entry -> {
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            GTVAction a;
            if (actions.size() == 1)
                a = actions.iterator().next().left;
            else {
                List<Pair<GTVAction, GTVState>> tmp = actions.stream().filter(y -> y.left instanceof GTVSendStar).collect(Collectors.toList());
                if (tmp.size() != 1) {
                    throw new RuntimeException("Shouldn't get in here: " + s);
                }
                a = tmp.iterator().next().left;
                }
            Role r;
            String paramA;
            if (a instanceof GTVSend cast) {
                r = cast.role;
                paramA = GTGenUtil.sendToParam(cast);
            } else if (a instanceof GTVSendStar cast) {
                r = cast.role;
                paramA = GTGenUtil.sendToParam(cast);
            } else {
                throw new RuntimeException("Unexpected action type in generateSelectAux.");
            }
            String sendName = "send_" + paramA;
            List<ErlTerm> sendParams = Arrays.asList(new ErlVar(r + "Pid"), new ErlVar("Data"));
            ErlSeq sendBody = new ErlSeq();
            sendBody.addExpression(new ErlMatch(new ErlVar("Counter"),
                    new ErlRecordAccess(new ErlVar("Data"), "state_data", "mc_counter_" + s.c)));
            sendBody.addExpression(new ErlCall("gen_statem", "cast", Arrays.asList(
                    new ErlVar(r + "Pid"),
                    new ErlTuple(Arrays.asList(
                            new ErlCall("self", Collections.emptyList()),
                            new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                            new ErlVar("Counter")
                    ))
            )));
            ErlFun sendFunc = new ErlFun(sendName);
            sendFunc.addClause(sendParams, sendBody);

            String stateFuncName = GTGenUtil.stateToFuncName(s);
            List<ErlTerm> stateParams = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                    new ErlVar("Data")
            );
            ErlSeq stateBody = new ErlSeq();
            stateBody.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            stateBody.addExpression(new ErlCall(new ErlVar("CallbackModule"), stateFuncName, Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                    new ErlVar("Data")
            )));
            ErlFun stateFunc = new ErlFun(stateFuncName);
            stateFunc.addClause(stateParams, stateBody);

            return Stream.of(sendFunc, stateFunc);
        }).collect(Collectors.toList());
    }

    //TODO: fix this: generates send error twice
    protected List<ErlFun> generateInternalMixed(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVSendStar);
        res.addAll(generateSelectAux(s, rhs));
        String funcName = GTGenUtil.stateToFuncName(s);

        res.addAll(rhs.entrySet().stream().flatMap(entry -> {
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            return actions.stream().filter(p -> p.left instanceof GTVSendStar).map(p -> {
                GTVSendStar a = (GTVSendStar) p.left;
                String paramA = GTGenUtil.sendToParam(a);
                Map<String, ErlTerm> fields = new LinkedHashMap<>();
                //TODO: add PIDs
                fields.put("mc_counter_" + s.c, new ErlVar("MC"));
                ErlRecordPattern pattern = new ErlRecordPattern("state_data", fields);

                List<ErlTerm> params = Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlVar(a.role.toString()+ "Pid"), new ErlAtom(paramA), new ErlVar("Counter"))),
                        new ErlMatch(pattern, new ErlVar("Data"))
                );
                ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp(">="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));

                ErlSeq bodySeq = new ErlSeq();
                bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                        new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
                bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName, Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlVar(a.role.toString()+ "Pid"), new ErlAtom(paramA))),
                        new ErlVar("Data")
                )));
                ErlFun clause = new ErlFun(funcName);
                clause.addClause(params, guard, bodySeq);
                return clause;
            });
        }).collect(Collectors.toList()));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(lhs.entrySet().stream().map(entry -> {
            GTVRecv e = (GTVRecv) entry.getKey().right;
            String paramA = GTGenUtil.eventToParam(e);
            Map<String, ErlTerm> fields = new HashMap<>();
//          TODO:  fields.put("alice_pid", new ErlVar("AlicePid"));
            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);
            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                    new ErlMatch(dataPattern, new ErlVar("Data"))
            );
            ErlSeq bodySeq = new ErlSeq();
            ErlRecordUpdate recordUpdate = new ErlRecordUpdate(new ErlVar("Data"), "state_data");
            recordUpdate.addField("mc_counter_" + s.c,
                    new ErlCall(new ErlOp("+"), Arrays.asList(new ErlVar("MC"), new ErlAtom("1")))
            );
            ErlMatch match = new ErlMatch(new ErlVar("NewData"), recordUpdate);
            bodySeq.addExpression(match);
            bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName, Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                    new ErlVar("NewData")
            )));
            ErlFun clause = new ErlFun(funcName);
            clause.addClause(params, bodySeq);
            return clause;
        }).collect(Collectors.toList()));
        res.add(genGC(s));
        return res;
    }

    protected List<ErlFun> generateExternalMixedOI(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVSend);
        res.addAll(genExtMixLHSAux(s, lhs));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(genExtMixRHSAux(s, rhs));
        res.add(genGC(s));
        return res;
    }

    protected List<ErlFun> genExtMixLHSAux(GTVState s,
                                           Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs) {
        String funcName = GTGenUtil.stateToFuncName(s);
        return lhs.entrySet().stream().flatMap(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            String paramA = GTGenUtil.eventToParam(key.right);
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            return actions.stream().map(p -> {
                List<ErlTerm> params = Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                        new ErlVar("Data")
                );
                ErlSeq bodySeq = new ErlSeq();
                bodySeq.addExpression(new ErlMatch(new ErlVar("NewData"),
                        new ErlAtom("Data#state_data{mc_counter_" + s.c + " = MC + 1}")));
                bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                        new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
                bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                        Arrays.asList(
                                new ErlVar("EventType"),
                                new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                                new ErlVar("NewData")
                        )));
                ErlFun clause = new ErlFun(funcName);
                clause.addClause(params, bodySeq);
                return clause;
            });
        }).collect(Collectors.toList());
    }

    protected List<ErlFun> genExtMixRHSAux(GTVState s,
                                           Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs) {
        String funcName = GTGenUtil.stateToFuncName(s);
        return rhs.entrySet().stream().map(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            GTVRecv e = (GTVRecv) key.right;
            String paramA = GTGenUtil.eventToParam(e);

            Map<String, ErlTerm> fields = new LinkedHashMap<>();
            //TODO: add PIDs
            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern pattern = new ErlRecordPattern("state_data", fields);

            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), new ErlAtom(paramA), new ErlVar("Counter"))),
                    new ErlMatch(pattern, new ErlVar("Data"))
            );
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp(">="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
            ErlSeq bodySeq = new ErlSeq();
            bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                    Arrays.asList(
                            new ErlVar("EventType"),
                            new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                            new ErlVar("Data")
                    )));
            ErlFun clause = new ErlFun(funcName);
            clause.addClause(params, guard, bodySeq);
            return clause;
        }).collect(Collectors.toList());
    }

    protected List<ErlFun> generateExternalMixedII(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(genExtMixLHSAux(s, lhs));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        res.addAll(genExtMixRHSAux(s, rhs));
        res.add(genGC(s));
        return res;
    }

    protected List<ErlFun> generateExternalMixedNotEntry(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(s, lhs));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhsTau =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(s, lhsTau));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        String funcName = GTGenUtil.stateToFuncName(s);
        res.addAll(rhs.entrySet().stream().map(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            GTVRecv e = (GTVRecv) key.right;
            String paramA = GTGenUtil.eventToParam(e);
            Map<String, ErlTerm> fields = new HashMap<>();
//          TODO:  fields.put("alice_pid", new ErlVar("AlicePid"));
            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);

            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), new ErlAtom(paramA), new ErlVar("Counter"))),
                    new ErlMatch(dataPattern, new ErlVar("Data"))
            );
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp(">="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
            ErlSeq bodySeq = new ErlSeq();
            bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                    Arrays.asList(
                            new ErlVar("EventType"),
                            new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                            new ErlVar("NewData")
                    )));
            ErlFun clause = new ErlFun(funcName);
            clause.addClause(params, guard, bodySeq);
            return clause;
        }).collect(Collectors.toList()));
        res.add(genGC(s));
        return res;
    }

    protected ErlFun genGC(GTVState s) {
        String funcName = GTGenUtil.stateToFuncName(s);
        ErlVar stateDataVar = new ErlVar("Data");

        List<ErlTerm> params = Arrays.asList(
                new ErlVar("_EventType"),
                new ErlTuple(Arrays.asList(new ErlVar("_Pid"), new ErlVar("_Label"), new ErlVar("_Counter"))),
                stateDataVar
        );
        ErlTuple result = new ErlTuple(Arrays.asList(new ErlAtom("keep_state"), stateDataVar));
        ErlFun gcFun = new ErlFun(funcName);
        gcFun.addClause(params, result);
        return gcFun;
    }
}
