package org.scribble.ext.gt.codegen.erlang;

import org.jetbrains.annotations.NotNull;
import org.scribble.core.type.name.Op;
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
    private static final String OUTPUT_DIR = "./generated";
    private static final String ERL_EXTENSION = ".erl";
    private static final String HRL_EXTENSION = ".hrl";

    /** Generate and write the Erlang code for the generic behaviour module to a file. */
    // GTLConfig rather that role for sigma.map.keySet()
    public void generateCode(String protocolName, GTLConfig r, GTEFSM efsm, Map<Integer, Set<Op>> explicitCommiting) throws IOException {
        Role role = r.self;
        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName.toString());
        Files.createDirectories(outputDirectory);
        String moduleName = "gen_" + role.toString().toLowerCase();
        String callbackModuleName = role.toString().toLowerCase();

        Path filePath = outputDirectory.resolve(moduleName + ERL_EXTENSION);
        FileWriter writer = new FileWriter(filePath);

        // Write record file
        Path recordPath = outputDirectory.resolve(callbackModuleName + HRL_EXTENSION);
        FileWriter recordWriter = new FileWriter(recordPath);
        recordWriter.write(generateStateDataRecord(efsm, r.sigma.map.keySet()));
        recordWriter.close();

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
        List<ErlFun> functions = new ArrayList<>();

        // Generate state functions for each state in the EFSM.
        for (GTVState s : efsm.S) {
            switch (GTGenUtil.getStateKind(efsm, s)) {
                case END:
                    break;
                case BRANCH: {
                    //needs gc labels
                    List<ErlFun> branchFuns = generateBranchAux(efsm, s);
                    functions.addAll(branchFuns);
                    break;
                }
                case SELECT: {
                    List<ErlFun> selectFuns = generateSelect(efsm, s);
                    functions.addAll(selectFuns);
                    break;
                }
                case INTERNAL_MIXED: {
                    //needs gc labels
                    List<ErlFun> mixedFuns = generateInternalMixed(efsm, s);
                    functions.addAll(mixedFuns);
                    break;
                }
                case EXTERNAL_MIXED_OI: {
                    //needs gc labels
                    List<ErlFun> extOIFuns = generateExternalMixedOI(efsm, s);
                    functions.addAll(extOIFuns);
                    break;
                }
                case EXTERNAL_MIXED_II: {
                    //needs gc labels
                    List<ErlFun> extIIFuns = generateExternalMixedII(efsm, s);
                    functions.addAll(extIIFuns);
                    break;
                }
                case EXTERNAL_MIXED_NOT_ENTRY: {
                    //needs gc labels
                    List<ErlFun> extNotEntryFuns = generateExternalMixedNotEntry(efsm, s);
                    functions.addAll(extNotEntryFuns);
                    break;
                }
            }
        }

        for (ErlFun f : functions) {
            exportNames.add(f.getName() + "/" + f.getArity());
        }

        // Write export lists.
        String exportsLine = "-export([" + String.join(", ", exportNames) + "]).";
        writer.writeLine(exportsLine);
        writer.writeLine("");

        writer.writeLine("-include(\"" + callbackModuleName + ".hrl\").");

        // Write state_data type
        writer.writeLine(GTErlGenUtil.genStateDataType(efsm, r.sigma.map.keySet()));
        writer.writeLine("");

        List<ErlFun> aggregatedFuns = GTErlGenUtil.aggregateTypeSpecs(functions);
        ErlFun initFun = genInitFunction(callbackModuleName, efsm.init, efsm);
        //filer for functions for state functions starting with s
        List<ErlFun> stateFunctions = aggregatedFuns.stream()
                .filter(f -> f.getName().matches("s\\d.*"))
                .collect(Collectors.toList());
        stateFunctions.add(initFun);
        writer.writeLine(generateCallbackTypes(stateFunctions));
        writer.writeLine("");

        // Generate start_link/2 function
        ErlFun startLinkFun = generateStartLinkFun(moduleName);
        startLinkFun.write(writer);
        writer.writeLine("");

        // Generate callback_mode/0 function
        ErlFun cbModeFun = genCallbackModeFunction();
        cbModeFun.write(writer);
        writer.writeLine("");

        // Generate init/1 function
        initFun.write(writer);
        writer.writeLine("");

        GTErlGenUtil.writeStateFunctions(writer, aggregatedFuns);


        // Generate code_change/4 function for hot code upgrades
        ErlFun codeChangeFun = genCodeChangeFunction();
        codeChangeFun.write(writer);
        writer.writeLine("");

        // Generate terminate/3 function for cleanup
        ErlFun terminateFun = genTerminateFunction();
        terminateFun.write(writer);
        writer.writeLine("");

        writer.close();
    }


    /**
     * Generates the common state data record string.
     *
     * @param efsm  The EFSM model.
     * @param roles The set of roles.
     * @return The record definition string.
     */
    protected String generateStateDataRecord(GTEFSM efsm, Set<Role> roles) {
        // Generate counter fields for every state in the EFSM.
        Set<String> counterFields = efsm.S.stream()
                .filter(s -> s.c > 0)
                .map(s -> "mc_counter_" + s.c + " = 0 :: integer()")
                .collect(Collectors.toCollection(LinkedHashSet::new));

        // Create pid fields for each role.
        Set<String> rolePidFields = roles.stream()
                .map(role -> role.toString().toLowerCase() + "_pid :: pid() | undefined")
                .collect(Collectors.toCollection(LinkedHashSet::new));

        List<String> allFields = new ArrayList<>();
        allFields.addAll(counterFields);
        allFields.addAll(rolePidFields);
        return "-record(state_data, {" + String.join(", ", allFields) + "}).";
    }

    /**
     * Generates the callback type definitions for all state functions in the given EFSM.
     * The callback type for each state function is derived from its spec string.
     *
     * @param stateFunctions List of state functions.
     * @return A string containing the Erlang callback type definitions.
     */
    public String generateCallbackTypes(List<ErlFun> stateFunctions) {

        // Use a LinkedHashSet to remove duplicates while preserving order.
        Set<String> callbackTypes = stateFunctions.stream()
                .map(ErlFun::getSpec)
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedHashSet::new));

        // Build the callback type definitions in Erlang syntax.
        StringBuilder sb = new StringBuilder();
        for (String spec : callbackTypes) {
            // Each spec should look like:
            //   myFunction(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, state_data()}
            // prepend "-callback " and append a trailing period.
            if (spec.startsWith("init")) {
                spec = spec.replace("{CallbackModule :: module(), Args :: list()}", "Args :: list()");
            }
            sb.append("-callback ").append(spec.trim());
            if (!spec.trim().endsWith(".")) {
                sb.append(".");
            }
            sb.append("\n");
        }
        return sb.toString();
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
        startLinkFun.setSpec("start_link(CallbackModule :: module(), Args :: list()) ->\n" +
                "    {ok, pid()} | {error, term()}");
        return startLinkFun;
    }


    /** Build the init/1 function, which initializes the gen_statem. */
    private ErlFun genInitFunction(String callbackModuleName, GTVState initState, GTEFSM efsm) {
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

        initFun.setSpec("init({CallbackModule :: module(), Args :: list()}) -> \n\t" +
                GTErlGenUtil.getNextStateReturnType(efsm, initState));
        return initFun;
    }

    /** Build the callback_mode/0 function (returns 'state_functions'). */
    private ErlFun genCallbackModeFunction() {
        ErlFun cbModeFun = new ErlFun("callback_mode");
        cbModeFun.addClause(Collections.emptyList(), null, new ErlAtom("state_functions"));
        cbModeFun.setSpec("callback_mode() -> state_functions");
        return cbModeFun;
    }

    /** Build code_change/4 function for handling code upgrades (no state change). */
    private ErlFun genCodeChangeFunction() {
        ErlFun codeChangeFun = new ErlFun("code_change");
        ErlVar vsnVar = new ErlVar("_Vsn");
        ErlVar stateNameVar = new ErlVar("_StateName");
        ErlVar stateDataVar = new ErlVar("StateData");
        ErlVar extraVar = new ErlVar("_Extra");
        ErlTuple result = new ErlTuple(Arrays.asList(new ErlAtom("ok"), stateDataVar));
        codeChangeFun.addClause(Arrays.asList(vsnVar, stateNameVar, stateDataVar, extraVar), null, result);
        codeChangeFun.setSpec("code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->\n" +
                "    {ok, state_data()}");
        return codeChangeFun;
    }

    /** Build terminate/3 function for cleanup when the state machine stops. */
    private ErlFun genTerminateFunction() {
        ErlFun termFun = new ErlFun("terminate");
        ErlVar reasonVar = new ErlVar("_Reason");
        ErlVar stateVar = new ErlVar("_State");
        ErlVar stateDataVar = new ErlVar("_StateData");
        termFun.addClause(Arrays.asList(reasonVar, stateVar, stateDataVar), null, new ErlAtom("ok"));
        termFun.setSpec("terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok");
        return termFun;
    }

    protected List<ErlFun> generateBranchAux(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        res.addAll(generateBranchAux(s, filt, m));
        if(s.c > 0)
            res.add(genGC(s, GTGenUtil.getEvents(m)));
        return res;
    }

    protected List<ErlFun> generateBranchAux(
            GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt, GTEFSM m) {
        return filt.entrySet().stream().flatMap(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            GTVRecv e = (GTVRecv) key.right;
            String funcName = GTGenUtil.stateToFuncName(s);
            String paramA = GTGenUtil.eventToParam(e);
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            Map<String, ErlTerm> fields = new LinkedHashMap<>();
            if(s.c > 0)
                fields.put("mc_counter_" + s.c, new ErlVar("MC"));

            List<ErlTerm> payloadVars = e.pay.elems.stream().map(elem ->
                            new ErlVar(elem.toString())).collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            return actions.stream().map(pair -> {
                ErlTuple patternTuple =  new ErlTuple(
                        Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple));
                ErlGuard guard = null;
                List<ErlTerm> headArgs;
                if(s.c > 0) {
                     guard = new ErlGuard(new ErlCall(new ErlOp("=:="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
                     patternTuple.addElement( new ErlVar("Counter"));
                     ErlRecordPattern pattern = new ErlRecordPattern("state_data", fields);
                     headArgs = Arrays.asList(
                        new ErlVar("EventType"),
                        patternTuple,
                        new ErlMatch(pattern, new ErlVar("Data"))
                );
                } else{
                    headArgs = Arrays.asList(
                        new ErlVar("EventType"),
                        patternTuple,
                        new ErlVar("Data")
                );
                }

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
                //TODO: specific specs for payloads
                String spec = funcName + "(" +
                        "term(), " +
                        "{pid(), {atom(), term()}}, " +
                        "state_data()) -> \n\t " +
                        GTErlGenUtil.getNextStateReturnType(m, pair.right);
                clauseFun.setSpec(spec);
                return clauseFun;
            });
        }).collect(Collectors.toList());
    }

    protected List<ErlFun> generateSelect(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        return generateSelectAux(s, filt, m);
    }

    // Takes both ! and !*
    protected List<ErlFun> generateSelectAux(
            GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt, GTEFSM m) {

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
            return actions.stream().flatMap(y -> {
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
                String spec = stateFuncName + "(" +
                        "EventType :: term(), " +
                        "{atom()}, " +
                        "state_data()) -> \n\t" +
                        GTErlGenUtil.getNextStateReturnType(m, y.right);
                stateFunc.setSpec(spec);
                return Stream.of(sendFunc, stateFunc);
            });
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

        String sendName = "send_" + "s" + s.id + "_" + paramA;
        List<ErlTerm> payloadVars = p.elems.stream()
                .map(elem -> new ErlVar(elem.toString()))
                .collect(Collectors.toList());
        // Build function head: [ <Role>Pid, Data ]
        ErlVar role = new ErlVar(r.toString() + "Pid");

        List<ErlTerm> sendParams = new ArrayList<>();
        sendParams.add(role);
        sendParams.addAll(payloadVars);
        sendParams.add(new ErlVar("Data"));

        // Build the body sequence.
        ErlSeq sendBody = new ErlSeq();

        // Instantiate Counter = Data#state_data.mc_counter_<s.c>
        if(s.c > 0)
            sendBody.addExpression(new ErlMatch(
                    new ErlVar("Counter"),
                    new ErlRecordAccess(new ErlVar("Data"), "state_data", "mc_counter_" + s.c)
            ));

        // Build the payload tuple by mapping each element of p to an ErlVar.

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
                secondElem
        ));

        if(s.c > 0)
            innerTuple.addElement(new ErlVar("Counter"));
        // Build the cast call: gen_statem:cast(<Role>Pid, InnerTuple)
        ErlCall castCall = new ErlCall(new ErlAtom("gen_statem"), "cast",
                Arrays.asList(role, innerTuple));
        sendBody.addExpression(castCall);

        ErlFun sendFunc = new ErlFun(sendName);
        sendFunc.addClause(sendParams, sendBody);

        StringBuilder specType = new StringBuilder();
        specType.append(sendName).append("(");
        specType.append(role).append(" :: pid()");
        for (ErlTerm elem : payloadVars) {
            specType.append(", ").append(elem).append(" :: term()");
        }
        specType.append(", ");
        if(s.c == 0)
            specType.append("_");
        specType.append("Data :: state_data()) -> ok");

        sendFunc.setSpec(specType.toString());
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
            //RHS of Internal mixed choice??
            return actions.stream().filter(p -> p.left instanceof GTVSendStar).map(p -> {
                GTVSendStar a = (GTVSendStar) p.left;
                String paramA = GTGenUtil.sendToParam(a);
                ErlFun sendFunc = genSendFun(s, paramA, p.left);
                Map<String, ErlTerm> fields = new HashMap<>();
                if(s.c > 0)
                    fields.put("mc_counter_" + s.c, new ErlVar("MC"));
                ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);

                List<ErlTerm> params = Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                        //new ErlVar("Data")
                        new ErlMatch(dataPattern, new ErlVar("Data"))
                );
                ErlSeq bodySeq = new ErlSeq();
                ErlRecordUpdate recordUpdate = new ErlRecordUpdate(new ErlVar("Data"), "state_data");
                if(s.c > 0){
                     recordUpdate.addField("mc_counter_" + s.c,
                             new ErlCall(new ErlOp("+"), Arrays.asList(new ErlVar("MC"), new ErlAtom("1")))
                     );
                }
                     ErlMatch match = new ErlMatch(new ErlVar("NewData"), recordUpdate);
                     bodySeq.addExpression(match);


                bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                        new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
                bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName, Arrays.asList(
                        new ErlVar("EventType"),
                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
//                        new ErlVar("Data")
                        new ErlVar("NewData")

                )));
                ErlFun clause = new ErlFun(funcName);
                clause.addClause(params, bodySeq);
                String rhsSpec = funcName + "(" +
                        "EventType :: term(), " +
                        "{atom()}, " +
                        "state_data()) -> \n\t " +
                        GTErlGenUtil.getNextStateReturnType(m, p.right);
                clause.setSpec(rhsSpec);
                return Stream.of(sendFunc, clause);
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
            if(s.c > 0)
                fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=:="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
            // Build the payload tuple by mapping each element of p to an ErlAtom.
            List<ErlTerm> payloadVars = e.pay.elems.stream()
                    .map(elem -> new ErlVar(elem.toString()))
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
        //     if(s.c > 0){
        //         recordUpdate.addField("mc_counter_" + s.c,
        //                 new ErlCall(new ErlOp("+"), Arrays.asList(new ErlVar("MC"), new ErlAtom("1")))
        //         );
        //     }
        //     ErlMatch match = new ErlMatch(new ErlVar("NewData"), recordUpdate);
        //     bodySeq.addExpression(match);
//            bodySeq.addExpression(new ErlVar("Data"));
            bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                    new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));
            bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName, Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+ "Pid"), payloadTuple)),
                //     new ErlVar("NewData")
                    new ErlVar("Data")
            )));
            ErlFun clause = new ErlFun(funcName);
            clause.addClause(params, guard, bodySeq);

            String lhsSpec = funcName + "(" +
                    "EventType :: term(), " +
                    "{pid(), {term()}, integer()}, " +
                    "state_data()) -> \n\t" +
                    GTErlGenUtil.getNextStateReturnType(m, entry.getValue().iterator().next().right);
            clause.setSpec(lhsSpec);
            return clause;
        }).collect(Collectors.toList()));
        if(s.c > 0)
            res.add(genGC(s, GTGenUtil.getEvents(m)));
        return res;
    }

    // ! |> ? -- events tau |> ? -- actions ! |> eps*
    protected List<ErlFun> generateExternalMixedOI(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVSend);
        res.addAll(genExtMixLHSAux(s, lhs, m));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(genExtMixRHSAux(s, rhs, m));
        if(s.c > 0)
            res.add(genGC(s, GTGenUtil.getEvents(m)));
        return res;
    }

    protected List<ErlFun> genExtMixLHSAux(GTVState s,
                                           Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs, GTEFSM m) {

        String funcName = GTGenUtil.stateToFuncName(s);
        return lhs.entrySet().stream()
                .flatMap(entry -> {
                    Pair<GTVState, GTVEvent> key = entry.getKey();
                    String paramA = GTGenUtil.eventToParam(key.right);
                    Set<Pair<GTVAction, GTVState>> actions = entry.getValue();

                    return actions.stream().flatMap(p -> {
                        ErlGuard guard = null;
                        List<ErlTerm> tupleElements = new ArrayList<>();
                        if(entry.getKey().right instanceof GTVRecv){
                            GTVRecv e = (GTVRecv) entry.getKey().right;
                            Map<String, ErlTerm> fields = new LinkedHashMap<>();

                            // Build the payload tuple by mapping each element of p to an ErlVar.
                            List<ErlTerm> payload = new ArrayList<>();
                            payload.add(new ErlAtom(paramA));

                            List<ErlTerm> payloadVars = e.pay.elems.stream()
                                    .map(elem -> new ErlVar(elem.toString()))
                                    .collect(Collectors.toList());

                            ErlTuple payloadTuple = new ErlTuple(payload);
                            payload.addAll(payloadVars);
                            guard = new ErlGuard(new ErlCall(new ErlOp("=:="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
                            tupleElements = Arrays.asList(
                                    new ErlVar(e.role.toString()+ "Pid"),
                                    new ErlTuple(payload),
                                    new ErlVar("Counter"));

                        }

                        Map<String, ErlTerm> fields = new LinkedHashMap<>();
                        if(s.c > 0)
                            fields.put("mc_counter_" + s.c, new ErlVar("MC"));
                        ErlRecordPattern dataPattern = new ErlRecordPattern("state_data", fields);


                        if (tupleElements.isEmpty()) {
                            tupleElements.add(new ErlAtom(paramA));
                        }

                        List<ErlTerm> params = Arrays.asList(
                                new ErlVar("EventType"),
                                new ErlTuple(tupleElements),
                                new ErlMatch(dataPattern, new ErlVar("Data"))
                        );

                        ErlSeq bodySeq = new ErlSeq();

                        // Update the counter: Data#state_data{mc_counter_<s.c> = MC + 1}
                        ErlVar data;
                        if(s.c > 0){
                            data = new ErlVar("NewData");
                            ErlRecordUpdate recordUpdate = new ErlRecordUpdate(new ErlVar("Data"), "state_data");
                            recordUpdate.addField("mc_counter_" + s.c,
                                    new ErlCall(new ErlOp("+"), Arrays.asList(new ErlVar("MC"), new ErlAtom("1")))
                            );
                            bodySeq.addExpression(new ErlMatch(data, recordUpdate));
                        } else {
                            data = new ErlVar("Data");
                        }


                        // Retrieve the callback module from the process dictionary (using erlang:get/1)
                        bodySeq.addExpression(new ErlMatch(new ErlVar("CallbackModule"),
                                new ErlCall("get", Arrays.asList(new ErlAtom("callback_module")))));

                        // Callback call: CallbackModule:funcName(EventType, {paramA}, NewData)
                        bodySeq.addExpression(new ErlCall(new ErlVar("CallbackModule"), funcName,
                                Arrays.asList(
                                        new ErlVar("EventType"),
//                                        new ErlTuple(Arrays.asList(new ErlAtom(paramA))),
                                        new ErlTuple( tupleElements.stream()
                                                .filter(elem -> !elem.toString().equals("Counter"))
                                                .collect(Collectors.toList())
                                        ),
                                data)));

                        ErlFun clause = new ErlFun(funcName);
                        clause.addClause(params, guard, bodySeq);
                        String lhsSpec = funcName + "(" +
                                "EventType :: term(), " +
                                "{pid(), {term()}, integer()}, " +
                                "state_data()) -> \n\t" +
                                GTErlGenUtil.getNextStateReturnType(m, entry.getValue().iterator().next().right);
                        clause.setSpec(lhsSpec);

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
                                           Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs, GTEFSM m) {
        String funcName = GTGenUtil.stateToFuncName(s);
        return rhs.entrySet().stream().map(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            GTVRecv e = (GTVRecv) key.right;
            String paramA = GTGenUtil.eventToParam(e);

            Map<String, ErlTerm> fields = new LinkedHashMap<>();
            //TODO: add PIDs
            if(s.c > 0)
                fields.put("mc_counter_" + s.c, new ErlVar("MC"));
            ErlRecordPattern pattern = new ErlRecordPattern("state_data", fields);
            List<ErlTerm> payloadVars = e.pay.elems.stream().map(elem ->
                            new ErlVar(elem.toString())).collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple, new ErlVar("Counter"))),
                    new ErlMatch(pattern, new ErlVar("Data"))
            );
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=:="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
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
            String lhsSpec = funcName + "(" +
                    "EventType :: term(), " +
                    "{pid(), {term()}, integer()}, " +
                    "state_data()) -> \n\t" +
                    GTErlGenUtil.getNextStateReturnType(m, entry.getValue().iterator().next().right);
            clause.setSpec(lhsSpec);
            return clause;
        }).collect(Collectors.toList());
    }

    // ! |> ? -- events tau |> ? -- actions ! |> eps* -- entry
    protected List<ErlFun> generateExternalMixedII(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(genExtMixLHSAux(s, lhs, m));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        res.addAll(genExtMixRHSAux(s, rhs, m));
        if(s.c > 0)
            res.add(genGC(s, GTGenUtil.getEvents(m)));
        return res;
    }

    protected List<ErlFun> generateExternalMixedNotEntry(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(s, lhs, m));
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhsTau =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(s, lhsTau, m));
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

            List<ErlTerm> payloadVars = e.pay.elems.stream().map(elem ->
                            new ErlVar(elem.toString())).collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(paramA));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("EventType"),
                    new ErlTuple(Arrays.asList(new ErlVar(e.role.toString()+"Pid"), payloadTuple, new ErlVar("Counter"))),
                    new ErlMatch(dataPattern, new ErlVar("Data"))
            );
            ErlGuard guard = new ErlGuard(new ErlCall(new ErlOp("=:="), Arrays.asList(new ErlVar("Counter"), new ErlVar("MC"))));
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
            String lhsSpec = funcName + "(" +
                    "EventType :: term(), " +
                    "{pid(), {term()}, integer()}, " +
                    "state_data()) -> \n\t" +
                    GTErlGenUtil.getNextStateReturnType(m, entry.getValue().iterator().next().right);
            clause.setSpec(lhsSpec);

            return clause;
        }).collect(Collectors.toList()));
        if(s.c > 0)
            res.add(genGC(s, GTGenUtil.getEvents(m)));
        return res;
    }

    // need s.c > 0 when calling
    protected ErlFun genGC(GTVState s, Set<GTVEvent> events) {
        String funcName = GTGenUtil.stateToFuncName(s);
        ErlVar stateDataVar = new ErlVar("Data");
        ErlFun gcFun = new ErlFun(funcName);
        ErlTuple result = new ErlTuple(List.of(new ErlAtom("keep_state"), stateDataVar));
        // list of GTVEvent events of type GTVRecv with payloads
        List<GTVRecv>  gcEventsPay = events.stream()
                .filter(event -> event instanceof GTVRecv && !((GTVRecv) event).pay.isEmpty())
                .map(event -> (GTVRecv) event)
                .collect(Collectors.toList());
        // list of GTVEvent events of type GTVRecv without payloads
        List<GTVRecv> gcEventsNoPay = events.stream()
                .filter(event -> event instanceof GTVRecv && ((GTVRecv) event).pay.isEmpty())
                .map(event -> (GTVRecv) event)
                .collect(Collectors.toList());
        if (!gcEventsNoPay.isEmpty()) {
            // Clause head: cast, {_Pid, Msg, _Counter}, Data
            List<ErlTerm> params = Arrays.asList(
                    new ErlVar("_EventType"),
                    new ErlTuple(List.of(new ErlVar("_Pid"), new ErlVar("Msg"), new ErlVar("_Counter"))),
                    stateDataVar
            );
                // Build the guard expression by OR-chaining comparisons:
                // Msg =:= {'event1}' orelse Msg =:= {'event2'} orelse ...
                ErlTerm guardExpr = gcEventsNoPay.stream()
                        .map(event -> new ErlCall(new ErlOp("=:="),
                                Arrays.asList(new ErlVar("Msg"),
                                        new ErlTuple(Collections.singletonList(new ErlAtom(GTGenUtil.eventToParam(event)))))))
                        .reduce((cond1, cond2) -> new ErlCall(new ErlOp("\n\t\torelse"),
                                Arrays.asList(cond1, cond2)))
                        .orElse(null);

            ErlGuard guard = new ErlGuard(guardExpr);
            gcFun.addClause(params, guard, result);

            String spec = funcName + "(" +
                    "EventType :: term(), " +
                    "term(), " +
                    "state_data()) -> {keep_state, state_data()}";
            gcFun.setSpec(spec);
        }
        // For each GTVRecv event with payload, we generate a clause with the specific event and
        // its payload as a tuple in the clause head for pattern matching.
        gcEventsPay.stream()
                .map(event -> {
                    // build the payload tuple: {eventName, PayloadElements…}
                    ErlTuple payloadTuple = new ErlTuple(
                            Stream.concat(
                                    Stream.of(new ErlAtom(GTGenUtil.eventToParam(event))),
                                    event.pay.elems.stream()
                                            .map(elem -> new ErlVar(elem.toString()))
                            ).collect(Collectors.toList())
                    );

                    // build clause parameters: {_EventType, {_Pid, {…}, _Counter}, Data}
                    return List.of(
                            new ErlVar("_EventType"),
                            new ErlTuple(List.of(new ErlVar("_Pid"), payloadTuple, new ErlVar("_Counter"))),
                            stateDataVar
                    );
                })
                .forEach(params -> gcFun.addClause(params, null, result));

        return gcFun;
    }


}

