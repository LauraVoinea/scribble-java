package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.Op;
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

public class GTCallbackModule {
    private static final String OUTPUT_DIR = "./generated";
    private static final String ERL_EXTENSION = ".erl";

    // GTLConfig rather that role for sigma.map.keySet()
    public void generate(String protocolName, GTLConfig r, GTEFSM efsm) throws IOException {
        Role role = r.self;
        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName.toString());
        Files.createDirectories(outputDirectory);
        String moduleName = role.toString().toLowerCase();
        String behaviourName = "gen_" + role.toString().toLowerCase();

        Path filePath = outputDirectory.resolve(moduleName + ERL_EXTENSION);
        FileWriter writer = new FileWriter(filePath);
        // Module declaration and behaviour
        writer.writeLine("-module(" + moduleName + ").");
        writer.writeLine("-behaviour(" + behaviourName + ").");
        writer.writeLine("");

        Set<String> exportNames = new LinkedHashSet<>();
        exportNames.add("init/1");
        exportNames.add("callback_mode/0");
        exportNames.add("start_link/0");

        // Prepare a list to store all generated state functions
        List<ErlFun> functions = new ArrayList<>();

        // Generate state functions for each state in the EFSM.
        for (GTVState s : efsm.S) {
            switch (GTGenUtil.getStateKind(efsm, s)) {
                case END:
                    break;
                case BRANCH: {
                    List<ErlFun> branchFuns = generateBranch(efsm, s);
                    functions.addAll(branchFuns);
                    break;
                }
                case SELECT: {
                    List<ErlFun> selectFuns = generateSelect(efsm, s, role);
                    functions.addAll(selectFuns);
                    break;
                }
                case INTERNAL_MIXED: {
                    List<ErlFun> mixedFuns = generateInternalMixed(efsm, s, role);
                    functions.addAll(mixedFuns);
                    break;
                }
                case EXTERNAL_MIXED_OI: {
                    List<ErlFun> extOIFuns = generateExternalMixedOI(efsm, s, role);
                    functions.addAll(extOIFuns);
                    break;
                }
                case EXTERNAL_MIXED_II: {
                    List<ErlFun> extIIFuns = generateExternalMixedII(efsm, s);
                    functions.addAll(extIIFuns);
                    break;
                }
                case EXTERNAL_MIXED_NOT_ENTRY: {
                    List<ErlFun> extNotEntryFuns = generateExternalMixedNotEntry(efsm, s, role);
                    functions.addAll(extNotEntryFuns);
                    break;
                }
            }
        }


//        return membs.stream().map(Object::toString).collect(Collectors.joining("\n\n"));
        for (ErlFun f : functions) {
            exportNames.add(f.getName() + "/" + f.getArity());
        }

        // Write export lists.
        String exportsLine = "-export([" + String.join(", ", exportNames) + "]).";
        writer.writeLine(exportsLine);
        writer.writeLine("");

        writer.writeLine("-include(\"" + moduleName + ".hrl\").");
        // Write state_data type
        writer.writeLine(GTErlGenUtil.genStateDataType(efsm, r.sigma.map.keySet()));
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
        ErlFun initFun = genInitFunction(role, efsm, efsm.init, r.sigma.map.keySet());
        initFun.write(writer);
        writer.writeLine("");
        List<ErlFun> aggregatedFuns = GTErlGenUtil.aggregateTypeSpecs(functions);
        GTErlGenUtil.writeStateFunctions(writer, aggregatedFuns);

        // Generate init/1 function
        ErlFun connFun = genConnectionFunction(role, efsm, r.sigma.map.keySet());
        connFun.write(writer);
        writer.writeLine("");
        writer.close();
    }


    private ErlFun generateStartLinkFun(String moduleName) {
        // The target function is:
        // start_link() ->
        //   gen_role:start_link(?MODULE, []).

        String funName = "start_link";
        List<ErlTerm> headArgs = List.of();


        ErlTerm moduleArg = new ErlVar("?MODULE");
        ErlTerm emptyList = new ErlList(Collections.emptyList());
        ErlCall startLinkCall = new ErlCall(new ErlAtom("gen_" + moduleName), "start_link",
                List.of(moduleArg, emptyList));

        ErlFun fun = new ErlFun(funName);
        fun.addClause(headArgs, startLinkCall);
        fun.setSpec("start_link() -> {ok, pid()} | {error, term()}");
        return fun;
    }


    /** Build the init/1 function, which initializes gen_role. */
    private ErlFun genInitFunction(Role self, GTEFSM efsm, GTVState initState, Set<Role> roles) {
        // Function head: init([]) ->
        List<ErlTerm> headArgs = List.of(new ErlList(Collections.emptyList()));

        // Build the function body as a sequence of expressions.
        ErlSeq bodySeq = new ErlSeq();
        // --- Create state data record.
        // Build a record update for state_data with:
        // - For each role (other than self) add a field <role>_pid bound to that role's PID variable.
        LinkedHashMap<String, ErlTerm> recFields = new LinkedHashMap<>();
        recFields.put("mc_counter_1", new ErlAtom("0")); // you could later compute this dynamically.
//        for (Role r : roles) {
//            if (r.equals(self))
//                continue;
//            String rName = r.toString().toLowerCase();
//            recFields.put(rName + "_pid", new ErlVar(rName + "Pid"));
//        }
        ErlRecordUpdate stateRecord = new ErlRecordUpdate(null, "state_data");
        recFields.forEach(stateRecord::addField);
        ErlMatch assignData = new ErlMatch(new ErlVar("Data"), stateRecord);
        bodySeq.addExpression(assignData);

        // --- Print an initialization message.
        ErlCall initFormat = new ErlCall("io", "format", List.of(
                new ErlString(self.toString().toLowerCase() + " initialized ~n"),
                new ErlList(Collections.emptyList())
        ));
        bodySeq.addExpression(initFormat);

        // Return tuple for init starts with ok rather than next_state
        ErlTerm retTuple = genNextState(efsm, initState);
        if (retTuple instanceof ErlTuple) {
            ErlTuple tuple = (ErlTuple) retTuple;
            List<ErlTerm> elements = new ArrayList<>(tuple.getElements());
            if (!elements.isEmpty() && elements.get(0) instanceof ErlAtom) {
                ErlAtom firstAtom = (ErlAtom) elements.get(0);
                if ("next_state".equals(firstAtom.getValue())) {
                    // Replace "next_state" with "ok"
                    elements.set(0, new ErlAtom("ok"));
                    retTuple = new ErlTuple(elements);
                }
            }
        }

        bodySeq.addExpression(retTuple);

        // Create the init function.
        ErlFun initFun = new ErlFun("init");
        initFun.addClause(headArgs, bodySeq);
        initFun.setSpec(initFun.getName() + "(list()) -> " + GTErlGenUtil.getNextStateReturnType(efsm, initState));
        return initFun;
    }

    private ErlFun genConnectionFunction(Role self, GTEFSM efsm, Set<Role> roles) {
        // Function head: connection() ->
        List<ErlTerm> headArgs = List.of();

        // Build the function body as a sequence of expressions.
        ErlSeq bodySeq = new ErlSeq();

        // --- Print a connection message.
        ErlCall connFormat = new ErlCall("io", "format", List.of(
                new ErlString(self.toString().toLowerCase() + " connected ~n"),
                new ErlList(Collections.emptyList())
        ));
        bodySeq.addExpression(connFormat);
        // --- Create state data record.
        // Build a record update for state_data with:
        // - For each role (other than self) add a field <role>_pid bound to that role's PID variable.
        LinkedHashMap<String, ErlTerm> recFields = new LinkedHashMap<>();

        // --- For each role other than self, generate a binding for that role's PID and send a message.
        for (Role r : roles) {
            if (r.equals(self))
                continue;
            // Assume role names are in lowercase (e.g. "bob")
            String rName = r.toString().toLowerCase();
            ErlVar rPidVar = new ErlVar(rName + "Pid");
            recFields.put(rName + "_pid", rPidVar);
            // Build the case expression: case whereis(r) of ... end.
            ErlCall whereisCall = new ErlCall("whereis", List.of(new ErlAtom(rName)));
            ErlCase caseExpr = new ErlCase(whereisCall);
            // Clause 1: when undefined
            ErlSeq undefinedSeq = new ErlSeq();
            ErlCall formatCallCase = new ErlCall("io", "format", List.of(
                    new ErlString(rName + " is not available yet. Will retry...~n"),
                    new ErlList(Collections.emptyList())
            ));
            undefinedSeq.addExpression(formatCallCase);
            ErlCall sleepCall = new ErlCall("timer", "sleep", List.of(new ErlAtom("1000")));
            undefinedSeq.addExpression(sleepCall);
            // Retry: whereis(r)
            ErlCall whereisCall2 = new ErlCall("whereis", List.of(new ErlAtom(rName)));
            undefinedSeq.addExpression(whereisCall2);
            caseExpr.addClause(new ErlAtom("undefined"), undefinedSeq);
            // Clause 2: pattern: Pid -> Pid
            caseExpr.addClause(new ErlVar("Pid_" + rName), new ErlVar("Pid_" + rName));

            // Bind the result of the case expression to rPidVar.
            ErlMatch assignRPid = new ErlMatch(rPidVar, caseExpr);
            bodySeq.addExpression(assignRPid);


        }
        ErlRecordUpdate stateRecord = new ErlRecordUpdate(null, "state_data");
        recFields.forEach(stateRecord::addField);
        bodySeq.addExpression(stateRecord);

        // Create the connection function.
        ErlFun connFun = new ErlFun("connection");
        connFun.addClause(headArgs, bodySeq);
        connFun.setSpec(connFun.getName() + "() -> {state_data()}");
        return connFun;
    }

    /** Build the callback_mode/0 function (returns 'state_functions'). */
    private ErlFun createCallbackModeFunction() {
        ErlFun cbModeFun = new ErlFun("callback_mode");
        cbModeFun.addClause(Collections.emptyList(), null, new ErlAtom("state_functions"));
        cbModeFun.setSpec("callback_mode() -> state_functions");
        return cbModeFun;
    }


    protected List<ErlFun> generateBranch(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateBranchAux(m, s, filt);
    }


    protected List<ErlFun> generateBranchAux(
            GTEFSM m, GTVState s,
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
        return edges.entrySet().stream().flatMap(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            // Expecting a receive event
            GTVRecv e = (GTVRecv) key.right;
            String funName = GTGenUtil.stateToFuncName(s);
            String paramA = GTGenUtil.eventToParam(e);
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();

            return actions.stream().map(pair -> {
                // Build the clause head:
                // 1. First argument: the mode, as a constant atom "cast".
                ErlTerm arg1 = new ErlAtom("cast");

                List<ErlTerm> payloadVars = e.pay.elems.stream().map(
                        elem -> new ErlVar(elem.toString())).
                        collect(Collectors.toList());
                List<ErlTerm> tupleElements = new ArrayList<>();
                tupleElements.add(new ErlAtom(paramA));
                tupleElements.addAll(payloadVars);

                ErlTuple payloadTuple = new ErlTuple(tupleElements);
                // 2. Second argument: a tuple {<Role>Pid, <paramA>}.
                ErlTerm arg2 = new ErlTuple(List.of(
                        new ErlVar(e.role.toString() + "Pid"),
                       payloadTuple
                ));
                // 3. Third argument: a match forcing Data to match a record pattern.
                Map<String, ErlTerm> fields = new LinkedHashMap<>();
                fields.put(e.role.toString().toLowerCase() + "_pid", new ErlVar(e.role.toString() + "Pid"));

                ErlRecordPattern recPattern = new ErlRecordPattern("state_data", fields);
                ErlTerm arg3 = new ErlMatch(recPattern, new ErlVar("Data"));
                List<ErlTerm> headArgs = List.of(arg1, arg2, arg3);

                // Build the clause body.
                // For now, we wrap the raw next-state expression in an ErlAtom.

                ErlTerm body = genNextState(m, pair.right);

                // Create a new function clause with the given head and body.
                ErlFun clause = new ErlFun(funName);
                clause.addClause(headArgs, body);
                String spec = funName + "(" +
                        "cast, " +
                        "{pid(), {atom(), term()}}, " +
                        "state_data()) -> " +
                        GTErlGenUtil.getNextStateReturnType(m, pair.right);
                clause.setSpec(spec);

                return clause;
            });
        }).collect(Collectors.toList());
    }


    protected List<ErlFun> generateSelect(GTEFSM m, GTVState s, Role self) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateSelectAux(m, s, filt, self);
    }


    protected List<ErlFun> generateSelectAux(
            GTEFSM m, GTVState s,
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges, Role self) {
        List<ErlFun> res = new LinkedList<>();
        //only generate make_choice_s if multiple edges
        if(edges.size() > 1) {
            res.add(genMakeChoice_s(s));
        }
        res.addAll(edges.entrySet().stream().flatMap(entry -> {
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            return actions.stream().map(y -> {
                // For each send action, assume it is a GTVSend.
                GTVSend a = (GTVSend) y.left;
                String funName = GTGenUtil.stateToFuncName(s);
                String paramA = GTGenUtil.sendToParam(a);
                // Build the clause head:
                // 1. First argument: the mode, as a constant atom "cast".
                ErlTerm arg1 = new ErlAtom("internal");
                // 2. Second argument: a tuple {<Role>Pid, <paramA>}.
                ErlTerm arg2 = new ErlTuple(List.of(
                        new ErlAtom(paramA)
                ));

                Map<String, ErlTerm> fields = new LinkedHashMap<>();
                fields.put(a.role.toString().toLowerCase() + "_pid", new ErlVar(a.role.toString() + "Pid"));

                ErlRecordPattern recPattern = new ErlRecordPattern("state_data", fields);
                ErlTerm arg3 = new ErlMatch(recPattern, new ErlVar("Data"));
                List<ErlTerm> headArgs = List.of(arg1, arg2, arg3);

                // Build the clause body as a sequence.
                ErlSeq bodySeq = new ErlSeq();
                ErlCall logSnd = new ErlCall("io", "format", List.of(
                        new ErlString("B: s" + s.id + " Sending " + paramA + " to " + a.role + " ~n"),
                        new ErlList(Collections.emptyList())
                ));
                bodySeq.addExpression(logSnd);
                // First expression: gen_role:send_<paramA>(<Role>Pid, paramA)
                String sendName = "send_" + "s" + s.id + "_" + paramA;
                ErlCall sendCall = new ErlCall(
                        new ErlAtom("gen_") + self.toString().toLowerCase(),
//                        "send_" + paramA,
                       sendName,
                        List.of(new ErlVar(a.role.toString() + "Pid"), new ErlVar("Data"))
                );
                bodySeq.addExpression(sendCall);
                // Second expression: the next state expression.
                ErlTerm nextStateExpr = genNextState(m, y.right);
                bodySeq.addExpression(nextStateExpr);

                // Create a new ErlFun clause with the given head and body.
                ErlFun clause = new ErlFun(funName);
                clause.addClause(headArgs, bodySeq);
                String spec = funName + "(" +
                        "internal, " +
                        "{atom()}, " +
                        "state_data()) -> " +
                        GTErlGenUtil.getNextStateReturnType(m, y.right);
                clause.setSpec(spec);
                return clause;
            });
        }).collect(Collectors.toList()));
        return res;
    }


    protected List<ErlFun> generateInternalMixed(GTEFSM m, GTVState s, Role self) {
        // Filter transitions for state s.
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        String funName = GTGenUtil.stateToFuncName(s);
        List<ErlFun> res = new LinkedList<>();

//        // !!! TODO missing ?/!* case
//        /*Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
//                filt.entrySet().stream().filter(x ->
//                        x.getValue().stream().anyMatch(y -> y.left instanceof GTVSendStar)).collect(
//                        Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));*/

        // --- Process RHS transitions (send-star branch)
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> tauMap =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        if (tauMap.size() != 1) {
            throw new RuntimeException("Unexpected internal mixed structure (tau branch): " + tauMap);
        }
        Set<Pair<GTVAction, GTVState>> sendStars = tauMap.values().iterator().next();
        if (sendStars.size() != 1) {
            throw new RuntimeException("Unexpected internal mixed structure (send-star branch): " + tauMap);
        }
        Pair<GTVAction, GTVState> sendStar = sendStars.iterator().next();
        GTVSendStar a = (GTVSendStar) sendStar.left;
        String paramA = GTGenUtil.sendToParam(a);

        // Build head for tau clause: [ internal, {paramA}, Data ]
        ErlTerm arg1 = new ErlAtom("internal");
        ErlTuple arg2 = new ErlTuple(List.of(new ErlAtom(paramA)));

        Map<String, ErlTerm> fields = new LinkedHashMap<>();
        fields.put(a.role.toString().toLowerCase() + "_pid", new ErlVar(a.role.toString() + "Pid"));
        ErlRecordPattern recPattern = new ErlRecordPattern("state_data", fields);
        ErlTerm arg3 = new ErlMatch(recPattern, new ErlVar("Data"));
        List<ErlTerm> tauHead = List.of(arg1, arg2, arg3);

        // Build body as a case expression:
        // Call: make_choice_<paramA>(<Role>Pid, Data)
        ErlCall makeChoiceCall = new ErlCall("make_choice_" + paramA,
                List.of(new ErlVar("Data")));
        ErlCase rhsCase = new ErlCase(makeChoiceCall);


        // Clause 1: Pattern "1" -> next state expression.
        rhsCase.addClause(new ErlAtom("1"),
                new ErlTuple(Arrays.asList(new ErlAtom("keep_state"), new ErlVar("Data"))));
        String sendName = "send_" + "s" + s.id + "_" + paramA;

        ErlCall formatCall = new ErlCall("io", "format", List.of(
                new ErlString(self.toString() + ": s" + s.id + " Sending " + paramA + " to " + a.role + " ~n"),
                new ErlList(Collections.emptyList())
        ));
        // Clause 2: Pattern "2" -> send call then next state.
        ErlCall rhsSendCall = new ErlCall(new ErlAtom("gen_" + self.toString().toLowerCase()), sendName,
                List.of(new ErlVar(a.role.toString() + "Pid"), new ErlVar("Data")));
        ErlSeq rhsBodySeq = new ErlSeq();
        rhsBodySeq.addExpression(formatCall);
        rhsBodySeq.addExpression(rhsSendCall);
        ErlTerm rhsNextState = genNextState(m, sendStar.right);
        rhsBodySeq.addExpression(rhsNextState);
        rhsCase.addClause(new ErlAtom("2"), rhsBodySeq);
        String rhsSpec = funName + "(" +
                "internal, " +
                "{atom()}, " +
                "state_data()) -> " +
                GTErlGenUtil.getNextStateReturnType(m, sendStar.right);
        ErlFun rhsSendFun = genMakeChoice_a(a.op);

        // Create the tau clause with head and body.
        ErlFun tauClause = new ErlFun(funName);
        tauClause.addClause(tauHead, rhsCase);
        tauClause.setSpec(rhsSpec);
        res.add(rhsSendFun);
        res.add(tauClause);

        // --- Process LHS external events (pattern ?a)
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> extMap =
                GTGenUtil.filterEdgesByEvent(filt, x -> x.getKind() == GTVEvent.Kind.EXTERNAL);

        List<ErlFun> externalClauses = extMap.entrySet().stream().flatMap(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            Set<Pair<GTVAction, GTVState>> vs = entry.getValue();
            GTVRecv e = (GTVRecv) key.right;

            // One clause per edge.
            if (vs.size() != 1 && vs.stream().filter(y -> y.left instanceof GTVEpsilon).count() != 1) {
                throw new RuntimeException("Unexpected external branch clause: " + key + " , " + vs);
            }
            Pair<GTVAction, GTVState> succ = vs.iterator().next();
            String a1 = GTGenUtil.eventToParam(e);

            List<ErlTerm> payloadVars = e.pay.elems.stream().map(
                    elem -> new ErlVar(elem.toString())).
                    collect(Collectors.toList());
            List<ErlTerm> tupleElements = new ArrayList<>();
            tupleElements.add(new ErlAtom(a1));
            tupleElements.addAll(payloadVars);

            ErlTuple payloadTuple = new ErlTuple(tupleElements);

            // Build head: [ cast, {<Role>Pid, {a1, payload}, Data ]
            ErlTerm headCast = new ErlAtom("cast");
            ErlTuple headExtTuple = new ErlTuple(List.of(
                    new ErlVar(e.role.toString() + "Pid"),
                    payloadTuple
            ));

            Map<String, ErlTerm> lhsFields = new LinkedHashMap<>();
            lhsFields.put(a.role.toString().toLowerCase() + "_pid", new ErlVar(a.role.toString() + "Pid"));
            ErlRecordPattern lhsRecPattern = new ErlRecordPattern("state_data", fields);
            ErlTerm lhsData = new ErlMatch(lhsRecPattern, new ErlVar("Data"));

            List<ErlTerm> extHead = List.of(headCast, headExtTuple, lhsData);

            // Generate the function for make_choice on external events.
            ErlFun sendFunc = genMakeChoice_a(e.op);

            ErlCall logRcv = new ErlCall("io", "format", List.of(
                    new ErlString(e.role.toString() + ": s" + s.id + " Received " + headExtTuple + " from " + a.role + " ~n"),
                    new ErlList(Collections.emptyList())
            ));

            // Build body as a case expression.
            // Call: make_choice_<a1>(Data)
            ErlCall extMakeChoiceCall = new ErlCall("make_choice_" + a1, List.of(new ErlVar("Data")));
            ErlCase extCase = new ErlCase(extMakeChoiceCall);
            // Clause 1: Pattern "1" -> next state expression.
            extCase.addClause(new ErlAtom("1"), genNextState(m, succ.right));
            // Clause 2: Pattern "2" -> send call then next state.
            //RHS choice; send RHS label

            ErlCall extSendCall = new ErlCall(new ErlAtom("gen_" + self.toString().toLowerCase()), sendName,
                    List.of(new ErlVar(a.role.toString() + "Pid"), new ErlVar("Data")));
            ErlSeq extBodySeq = new ErlSeq();
            extBodySeq.addExpression(formatCall);
            extBodySeq.addExpression(extSendCall);
            //TODO: RHS continuation
            extBodySeq.addExpression(rhsNextState);
            extCase.addClause(new ErlAtom("2"), extBodySeq);

            ErlFun extClause = new ErlFun(funName);
            extClause.addClause(extHead, extCase);
            String lhsSpec = funName + "(" +
                    "EventType :: term(), " +
                    "{pid(), {term()}, integer()}, " +
                    "state_data()) -> " +
                    GTErlGenUtil.getNextStateReturnType(m, entry.getValue().iterator().next().right);
            extClause.setSpec(lhsSpec);
            // Return both the send function and the clause.
            return Stream.of(sendFunc, extClause);
        }).collect(Collectors.toList());

        res.addAll(externalClauses);
        return res;
    }


    protected List<ErlFun> generateExternalMixedOI(GTEFSM m, GTVState s, Role self) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(m, s, lhs, self));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVRecv);
        res.addAll(generateBranchAux(m, s, rhs));

        return res;
    }

    protected List<ErlFun> generateExternalMixedII(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(m, s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilonStar);
        res.addAll(generateBranchAux(m, s, rhs));

        return res;
    }

    protected List<ErlFun> generateExternalMixedNotEntry(GTEFSM m, GTVState s, Role self) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(m, s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs_tau =  // !!! -- ! |> ?
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(m, s, lhs_tau, self));

        return res;
    }


    protected ErlFun genMakeChoice_s(GTVState s) {
        String funName = "make_choice_" + GTGenUtil.stateToFuncName(s);

        ErlVar dataVar = new ErlVar("_Data");
        List<ErlTerm> parameters = List.of(dataVar);

        ErlCall uniformCall = new ErlCall(
                new ErlAtom("rand"),
                "uniform",
                List.of(new ErlInteger(2))
        );

        // Create a new Erlang function representation and add the clause.
        ErlFun makeChoiceFunction = new ErlFun(funName);
        makeChoiceFunction.addClause(parameters, uniformCall);
        makeChoiceFunction.setSpec(funName + "(state_data()) -> integer()");
        return makeChoiceFunction;
    }


    // !!! pay?  -- ! and !*
    protected ErlFun genMakeChoice_a(Op op) {
        String funName = "make_choice_" + op;

        ErlVar dataVar = new ErlVar("_Data");
        List<ErlTerm> parameters = List.of(dataVar);

        ErlCall uniformCall = new ErlCall(
                new ErlAtom("rand"),
                "uniform",
                List.of(new ErlInteger(2))
        );

        // Create a new Erlang function representation and add the clause.
        ErlFun makeChoiceFunction = new ErlFun(funName);
        makeChoiceFunction.addClause(parameters, uniformCall);
        makeChoiceFunction.setSpec(funName + "(state_data()) -> integer()");

        return makeChoiceFunction;
    }


    protected ErlTerm genNextState(GTEFSM m, GTVState succ) {
        switch (GTGenUtil.getStateKind(m, succ)) {
            case END:
                return new ErlTuple(List.of(
                        new ErlAtom("stop"),
                        new ErlAtom("normal"),
                        new ErlVar("Data")
                ));
            case SELECT:
            case INTERNAL_MIXED:
            case EXTERNAL_MIXED_OI: {
                // Get all transitions from the successor state.
                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                        GTGenUtil.filterEdgesByState(m, succ);
                String sName = GTGenUtil.stateToFuncName(succ);
                // Filter for transitions whose event is a GTVTau.
                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> tauTransitions =
                        filt.entrySet().stream()
                                .filter(e -> e.getKey().right instanceof GTVTau)
                                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
                // If exactly one tau transition exists then use that tau's event parameter.
                if (tauTransitions.size() == 1) {
                    Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> entry =
                            tauTransitions.entrySet().iterator().next();
                        GTVTau tau = (GTVTau) entry.getKey().right;
                        String a = GTGenUtil.eventToParam(tau);
                        // Return a tuple with the next state and the extra list.
                        return new ErlTuple(List.of(
                                new ErlAtom("next_state"),
                                new ErlAtom(sName),
                                new ErlVar("Data"),
                                new ErlList(List.of(
                                        new ErlTuple(List.of(
                                                new ErlAtom("next_event"),
                                                new ErlAtom("internal"),
                                                new ErlTuple(List.of(new ErlAtom(a)))
                                        ))
                                ))
                        ));
                }
                // Otherwise, build a case expression.
                //TODO: 1 or 2
                ErlCall makeChoiceCall = new ErlCall("make_choice_" + sName, List.of(new ErlVar("Data")));
                ErlCase caseExpr = new ErlCase(makeChoiceCall);
                tauTransitions.forEach((key, actions) -> {
                    GTVTau tau = (GTVTau) key.right;
                    String a = GTGenUtil.eventToParam(tau);
                    ErlAtom clausePattern = new ErlAtom(a);
                    ErlTuple bodyTuple = new ErlTuple(List.of(
                            new ErlAtom("next_state"),
                            new ErlAtom(sName),
                            new ErlVar("Data"),
                            new ErlList(List.of(
                                    new ErlTuple(List.of(
                                            new ErlAtom("next_event"),
                                            new ErlAtom("internal"),
                                            new ErlTuple(List.of(new ErlAtom(a)))
                                    ))
                            ))
                    ));
                    caseExpr.addClause(clausePattern, bodyTuple);
                });
                return caseExpr;
            }
            case BRANCH:
            case EXTERNAL_MIXED_II:
            case EXTERNAL_MIXED_NOT_ENTRY:
                return new ErlTuple(List.of(
                        new ErlAtom("next_state"),
                        new ErlAtom(GTGenUtil.stateToFuncName(succ)),
                        new ErlVar("Data")
                ));
        }
        throw new RuntimeException("Shouldn't get here?");
    }


}


