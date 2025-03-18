package org.scribble.ext.gt.codegen.erlang;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.GProtoName;
import org.scribble.core.type.name.Role;
import org.scribble.core.type.session.Payload;
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

        Set<String> exportNames = new LinkedHashSet<>();
        exportNames.add("init/1");
        exportNames.add("callback_mode/0");
        exportNames.add("code_change/4");
        exportNames.add("terminate/3");
        exportNames.add("start_link/2");

        // Prepare a list to store all generated state functions
        List<ErlFun> stateFunctions = new ArrayList<>();

        // Generate state functions for each state in the EFSM.
        for (GTVState s : efsm.S) {
            switch (GTGenUtil.getStateKind(efsm, s)) {
                case END:
                    break;
                case BRANCH: {
                    List<ErlFun> branchFuns = generateBranchAux(efsm, s);
                    stateFunctions.addAll(branchFuns);
                    break;
                }
                case SELECT: {
                    List<ErlFun> selectFuns = generateSelect(efsm, s);
                    stateFunctions.addAll(selectFuns);
                    break;
                }
                case INTERNAL_MIXED: {
                    List<ErlFun> mixedFuns = generateInternalMixed(efsm, s);
                    stateFunctions.addAll(mixedFuns);
                    break;
                }
                case EXTERNAL_MIXED_OI: {
                    List<ErlFun> extOIFuns = generateExternalMixedOI(efsm, s);
                    stateFunctions.addAll(extOIFuns);
                    break;
                }
                case EXTERNAL_MIXED_II: {
                    List<ErlFun> extIIFuns = generateExternalMixedII(efsm, s);
                    stateFunctions.addAll(extIIFuns);
                    break;
                }
                case EXTERNAL_MIXED_NOT_ENTRY: {
                    List<ErlFun> extNotEntryFuns = generateExternalMixedNotEntry(efsm, s);
                    stateFunctions.addAll(extNotEntryFuns);
                    break;
                }
            }
        }

        for (ErlFun f : stateFunctions) {
            exportNames.add(f.getName() + "/" + f.getArity());
        }

        // Write export lists.
        String exportsLine = "-export([" + String.join(", ", exportNames) + "]).";
        writer.writeLine(exportsLine);
        writer.writeLine("");

        writer.writeLine(generateStateDataRecord(efsm, r.sigma.map.keySet()));
        writer.writeLine("");

        // Generate start_link/2 function
        ErlFun startLinkFun = generateStartLinkFun(moduleName);
        startLinkFun.write(writer);
        writer.writeLine("");

        // Generate callback_mode/0 function
        ErlFun cbModeFun = createCallbackModeFunction();
        cbModeFun.write(writer);
        writer.writeLine("");

        // Generate init/1 function
        ErlFun initFun = createInitFunction(callbackModuleName, efsm.init);
        initFun.write(writer);
        writer.writeLine("");

        // Group state function clauses by name.
        Map<String, List<ErlFun>> groupedStateFunctions = stateFunctions.stream()
                .collect(Collectors.groupingBy(ErlFun::getName));

        // Write state functions
        for (Map.Entry<String, List<ErlFun>> entry : groupedStateFunctions.entrySet()) {
            String funcName = entry.getKey();
            // Aggregate all clauses for this function.
            ErlFun aggregated = new ErlFun(funcName);
            for (ErlFun clauseFun : entry.getValue()) {
                for (ErlFun.FunClause fc : clauseFun.getClauses()) {
                    aggregated.addClause(fc.args, fc.guard, fc.body);
                }
            }
            writer.writeLine("%% State function: " + funcName);
            aggregated.write(writer);
            writer.writeLine("");
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


    private ErlFun generateStartLinkFun(String moduleName) {
        String funcName = "start_link";
        List<ErlTerm> headParams = Arrays.asList(new ErlVar("CallbackModule"), new ErlVar("Args"));
        ErlCall ensureCall = new ErlCall("code", "ensure_loaded", Arrays.asList(new ErlVar("CallbackModule")));
        ErlCase caseExpr = new ErlCase(ensureCall);

        // Clause 1:
        // Pattern: {module, CallbackModule}
        ErlTuple pattern1 = new ErlTuple(Arrays.asList(new ErlAtom("module"), new ErlVar("CallbackModule")));
        // Body: gen_statem:start_link({local, CallbackModule}, gen_alice, {CallbackModule, Args}, [])
        ErlTuple arg1 = new ErlTuple(Arrays.asList(new ErlAtom("local"), new ErlVar("CallbackModule")));
        ErlAtom arg2 = new ErlAtom(moduleName);
        ErlTuple arg3 = new ErlTuple(Arrays.asList(new ErlVar("CallbackModule"), new ErlVar("Args")));
        ErlList arg4 = new ErlList(Collections.emptyList());
        ErlCall startLinkCall = new ErlCall(new ErlAtom("gen_statem"), "start_link",
                Arrays.asList(arg1, arg2, arg3, arg4));
        caseExpr.addClause(pattern1, startLinkCall);


        ErlTuple pattern2 = new ErlTuple(Arrays.asList(new ErlAtom("error"), new ErlVar("Reason")));
        ErlTuple errorResult = new ErlTuple(Arrays.asList(new ErlAtom("error"), new ErlVar("Reason")));
        caseExpr.addClause(pattern2, errorResult);

        ErlFun startLinkFun = new ErlFun(funcName);
        startLinkFun.addClause(headParams, caseExpr);

        return startLinkFun;
    }


    private String generateStateDataRecord(GTEFSM efsm, Set<Role> roles) {
        // Generate counter fields for every state in the EFSM.
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
        // The clause head should match a tuple: {CallbackModule, _Args}
        ErlTuple head = new ErlTuple(Arrays.asList(
                new ErlVar("CallbackModule"),
                new ErlVar("_Args")
        ));
        List<ErlTerm> headArgs = Arrays.asList(head);

        ErlSeq bodySeq = new ErlSeq();
        ErlCall formatCall = new ErlCall("io", "format", Arrays.asList(
                new ErlString(callbackModuleName + ": Initializing with callback module ~p~n"),
                new ErlList(Arrays.asList(new ErlVar("CallbackModule")))
        ));
        bodySeq.addExpression(formatCall);

        // put(callback_module, CallbackModule)
        ErlCall putCall = new ErlCall("put", Arrays.asList(
                new ErlAtom("callback_module"),
                new ErlVar("CallbackModule")
        ));
        bodySeq.addExpression(putCall);

        //CallbackModule:init([])
        ErlCall initCall = new ErlCall(new ErlVar("CallbackModule"), "init", Arrays.asList(
                new ErlList(Collections.emptyList())
        ));
        bodySeq.addExpression(initCall);

        // Create the init function and add the single clause.
        ErlFun initFun = new ErlFun("init");
        initFun.addClause(headArgs, bodySeq);
        return initFun;
    }

    /** Build the callback_mode/0 function (returns 'state_functions'). */
    private ErlFun createCallbackModeFunction() {
        ErlFun cbModeFun = new ErlFun("callback_mode");
        cbModeFun.addClause(Collections.emptyList(), null, new ErlAtom("state_functions"));
        return cbModeFun;
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

            List<ErlTerm> payloadVars = e.pay.elems.stream().map(elem -> new ErlAtom(elem.toString())).
                    collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            return actions.stream().map(pair -> {
                List<ErlTerm> headArgs = Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple, new ErlVar("Counter"))),
                        new ErlMatch(pattern, new ErlVar("Data"))
                );

                ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
                ErlSeq bodySeq = new ErlSeq();
                bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                        new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
                bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                        Arrays.asList(
                                new ErlVar("EventType"),
                                new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple)),
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

            String paramA;
            //TODO: refactor this;
            // do we need GTVSendStar?
            ErlFun sendFunc;
            if (a instanceof GTVSendStar cast) {
                paramA = GTGenUtil.sendToParam(cast);
                sendFunc = genSendFun(s, paramA, a);
            } else if (a instanceof GTVSend cast) {
                paramA = GTGenUtil.sendToParam(cast);
                sendFunc = genSendFun(s, paramA, a);
            } else {
                throw new RuntimeException("Unexpected action type in generateSelectAux.");
            }

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

    private static @NotNull ErlFun genSendFun(GTVState s, String paramA, GTVAction action) {
        // Extract role and payload from the action.
        Role r = null;
        Payload p = null;
        if (action instanceof GTVSendStar) {
            r = ((GTVSendStar) action).role;
            p = ((GTVSendStar) action).pay;
        } else if (action instanceof GTVSend) {
            r = ((GTVSend) action).role;
            p = ((GTVSend) action).pay;
        }

        String sendName = "send_" + paramA;

        // Build function head: [ <Role>Pid, Data ]
        List<ErlTerm> sendParams = Arrays.asList(
                new ErlVar(r.toString() + "Pid"),
                new ErlVar("Data")
        );

        // Build the body sequence.
        ErlSeq sendBody = new ErlSeq();

        // Instantiate Counter = Data#state_data.mc_counter_<s.c>
        sendBody.addExpression(new ErlMatch(
                new ErlVar("Counter"),
                new ErlRecordAccess(new ErlVar("Data"), "state_data", "mc_counter_" + s.c)
        ));

        // Build the payload tuple by mapping each element of p to an ErlAtom.
        List<ErlTerm> payloadVars = p.elems.stream()
                .map(elem -> new ErlAtom(elem.toString()))
                .collect(Collectors.toList());
        // Build a tuple for the payload: {paramA, PayloadElements...}
        // Prepend the parameter as an atom.
        List<ErlTerm> secondTupleElements = new LinkedList<>();
        secondTupleElements.add(new ErlAtom(paramA));
        secondTupleElements.addAll(payloadVars);
        ErlTuple secondElem = new ErlTuple(secondTupleElements);

        // Build the inner tuple: {self(), {paramA, ...}, Counter}
        ErlCall selfCall = new ErlCall("self", Collections.emptyList());
        ErlTuple innerTuple = new ErlTuple(Arrays.asList(
                selfCall,
                secondElem,
                new ErlVar("Counter")
        ));

        // Build the cast call: gen_statem:cast(<Role>Pid, InnerTuple)
        ErlCall castCall = new ErlCall(new ErlAtom("gen_statem"), "cast",
                Arrays.asList(new ErlVar(r.toString() + "Pid"), innerTuple));
        sendBody.addExpression(castCall);

        ErlFun sendFunc = new ErlFun(sendName);
        sendFunc.addClause(sendParams, sendBody);
        return sendFunc;
    }


    protected List<ErlFun> generateInternalMixed(GTEFSM m, GTVState s) {
        // Filter all transitions originating from state 's'.
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
//        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
//                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVSendStar);
        // filter by GTVTau, since (12[] ▷1, A?a1())=[(ε, 13[] 1), (A!*Timeout, 10[] 1)] is handled by the same
        // clause as LHS both in the API and in the role code
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
            String funcName = GTGenUtil.stateToFuncName(s);

        List<Stream<ErlFun>> rhsClauses = rhs.entrySet().stream().flatMap(entry -> {
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();

            return actions.stream().filter(p -> p.left instanceof GTVSendStar).map(p -> {
                GTVSendStar a = (GTVSendStar) p.left;
                String paramA = GTGenUtil.sendToParam(a);
                ErlFun sendFunc = genSendFun(s, paramA, p.left);
                List<ErlTerm> params = Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                        new ErlVar("Data")
                );

                ErlSeq bodySeq = new ErlSeq();
                bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                        new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
                bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName, Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                        new ErlVar("Data")
                )));
                ErlFun clause = new ErlFun(funcName);
                clause.addClause(params, bodySeq);
                return Stream.of(sendFunc, clause);
//                return Arrays.asList(sendFunc, clause).stream();
//                return Collections.singletonList(clause);
//                return clause;
            });
        }).collect(Collectors.toList());

        res.addAll(rhsClauses.stream().flatMap(x -> x).collect(Collectors.toList()));

        //LHS of Internal mixed choice
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(lhs.entrySet().stream().map(entry -> {
            GTVRecv e = (GTVRecv) entry.getKey().right;
            String paramA = GTGenUtil.eventToParam(e);
            Map<String, ErlTerm> fields = new HashMap<>();
//          TODO:  fields.put("alice_pid", new ErlVar("AlicePid"));
            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
            // Build the payload tuple by mapping each element of p to an ErlAtom.
            List<ErlTerm> payloadVars = e.pay.elems.stream()
                    .map(elem -> new ErlAtom(elem.toString()))
                    .collect(Collectors.toList());

            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);
            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+ "Pid"), payloadTuple, new ErlVar("Counter"))),
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
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+ "Pid"), payloadTuple)),
                    new ErlVar("NewData")
            )));
            ErlFun clause = new ErlFun(funcName);
            clause.addClause(params, guard, bodySeq);
            return clause;
        }).collect(Collectors.toList()));
        res.add(genGC(s));
        return res;
    }

    // ! |> ? -- events tau |> ? -- actions ! |> eps*
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
        return lhs.entrySet().stream()
                .flatMap(entry -> {
                    Pair<GTVState, GTVEvent> key = entry.getKey();
                    String paramA = GTGenUtil.eventToParam(key.right);
                    Set<Pair<GTVAction, GTVState>> actions = entry.getValue();

                    return actions.stream().flatMap(p -> {
                        Map<String, ErlTerm> fields = new LinkedHashMap<>();
                        // TODO: add PIDs?
                        fields.put("mc_counter_" + s.c, new ErlVar("MC"));
                        ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);

                        List<ErlTerm> params = Arrays.asList(
                                new ErlVar("EventType"),
                                new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                                new ErlMatch(dataPattern, new ErlVar("Data"))
                        );
                        ErlSeq bodySeq = new ErlSeq();

                        // Update the counter: Data#state_data{mc_counter_<s.c> = MC + 1}
                        ErlRecordUpdate recordUpdate = new ErlRecordUpdate(new ErlVar("Data"), "state_data");
                        recordUpdate.addField("mc_counter_" + s.c,
                                new ErlCall(new ErlOp("+"), Arrays.asList(new ErlVar("MC"), new ErlAtom("1")))
                        );
                        bodySeq.addExpression(new ErlMatch(new ErlVar("NewData"), recordUpdate));

                        // Retrieve the callback module from the process dictionary (using erlang:get/1)
                        bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                                new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));

                        // Callback call: CallbackModule:funcName(EventType, {paramA}, NewData)
                        bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                                Arrays.asList(
                                        new ErlVar("EventType"),
                                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                                        new ErlVar("NewData")
                                )));

                        ErlFun clause = new ErlFun(funcName);
                        clause.addClause(params, bodySeq);

                        // If the action is of type GTVSend, generate send function.
                        if (p.left instanceof GTVSend) {
                            ErlFun sendFunc = genSendFun(s, paramA, p.left);
                            return Stream.of(sendFunc, clause);
                        } else {
                            return Stream.of(clause);
                        }
                    });
                })
                .collect(Collectors.toList());
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
            List<ErlTerm> payloadVars = e.pay.elems.stream().map(elem -> new ErlAtom(elem.toString())).
                    collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple, new ErlVar("Counter"))),
                    new ErlMatch(pattern, new ErlVar("Data"))
            );
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
            ErlSeq bodySeq = new ErlSeq();
            bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                    Arrays.asList(
                            new ErlVar("EventType"),
                            new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple)),
                            new ErlVar("Data")
                    )));
            ErlFun clause = new ErlFun(funcName);
            clause.addClause(params, guard, bodySeq);
            return clause;
        }).collect(Collectors.toList());
    }

    //TODO: what is generateExternalMixedII?
    // still seems output input
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
//          TODO:  fields.put("role_pid", new ErlVar("RolePid"));
            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);

            List<ErlTerm> payloadVars = e.pay.elems.stream().map(elem -> new ErlAtom(elem.toString())).
                    collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple, new ErlVar("Counter"))),
                    new ErlMatch(dataPattern, new ErlVar("Data"))
            );
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
            ErlSeq bodySeq = new ErlSeq();
            bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                    Arrays.asList(
                            new ErlVar("EventType"),
                            new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple)),
                            new ErlVar("Data")
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
