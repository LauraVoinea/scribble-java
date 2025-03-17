package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.GProtoName;
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
    private static final String OUTPUT_DIR = "./test";
    private static final String ERL_EXTENSION = ".erl";

    public void generate(GProtoName protocolName, GTLConfig r, GTEFSM efsm) throws IOException {
        Role role = r.self;
        Path outputDirectory = Paths.get(OUTPUT_DIR, protocolName.toString());
        Files.createDirectories(outputDirectory);
        String moduleName = role.toString().toLowerCase();
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
        exportNames.add("start_link/0");

        // Prepare a list to store all generated state functions
        List<ErlFun> stateFunctions = new ArrayList<>();

        // Generate state functions for each state in the EFSM.
        for (GTVState s : efsm.S) {
            switch (GTGenUtil.getStateKind(efsm, s)) {
                case END:
                    break;
                case BRANCH: {
                    List<ErlFun> branchFuns = generateBranch(efsm, s);
                    stateFunctions.addAll(branchFuns);
                    break;
                }
                case SELECT: {
                    List<ErlFun> selectFuns = generateSelect(efsm, s, role);
                    stateFunctions.addAll(selectFuns);
                    break;
                }
                case INTERNAL_MIXED: {
                    List<ErlFun> mixedFuns = generateInternalMixed(efsm, s, role);
                    stateFunctions.addAll(mixedFuns);
                    break;
                }
                case EXTERNAL_MIXED_OI: {
                    List<ErlFun> extOIFuns = generateExternalMixedOI(efsm, s, role);
                    stateFunctions.addAll(extOIFuns);
                    break;
                }
                case EXTERNAL_MIXED_II: {
                    List<ErlFun> extIIFuns = generateExternalMixedII(efsm, s);
                    stateFunctions.addAll(extIIFuns);
                    break;
                }
                case EXTERNAL_MIXED_NOT_ENTRY: {
                    List<ErlFun> extNotEntryFuns = generateExternalMixedNotEntry(efsm, s, role);
                    stateFunctions.addAll(extNotEntryFuns);
                    break;
                }
            }
        }


//        return membs.stream().map(Object::toString).collect(Collectors.joining("\n\n"));
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
        ErlFun initFun = createInitFunction(role, efsm, efsm.init, r.sigma.map.keySet());
        initFun.write(writer);
        writer.writeLine("");

        // Group state function clauses by name.
        Map<String, List<ErlFun>> groupedStateFunctions = stateFunctions.stream()
                .collect(Collectors.groupingBy(ErlFun::getName));

        // Write state functions
        for (Map.Entry<String, List<ErlFun>> entry : groupedStateFunctions.entrySet()) {
            String funName = entry.getKey();
            // Aggregate all clauses for this function.
            ErlFun aggregated = new ErlFun(funName);
            for (ErlFun clauseFun : entry.getValue()) {
                for (ErlFun.FunClause fc : clauseFun.getClauses()) {
                    aggregated.addClause(fc.args, fc.guard, fc.body);
                }
            }
            writer.writeLine("%% State function: " + funName);
            aggregated.write(writer);
            writer.writeLine("");
        }

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

        return fun;
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


    /** Build the init/1 function, which initializes gen_role. */
    private ErlFun createInitFunction(Role self, GTEFSM efsm, GTVState initState, Set<Role> roles) {
        // Function head: init([]) ->
        List<ErlTerm> headArgs = List.of(new ErlList(Collections.emptyList()));

        // Build the function body as a sequence of expressions.
        ErlSeq bodySeq = new ErlSeq();

        // --- For each role other than self, generate a binding for that role's PID and send a message.
        for (Role r : roles) {
            if (r.equals(self))
                continue;
            // Assume role names are in lowercase (e.g. "bob")
            String rName = r.toString().toLowerCase();
            ErlVar rPidVar = new ErlVar(rName + "Pid");

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
            caseExpr.addClause(new ErlVar("Pid"), new ErlVar("Pid"));

            // Bind the result of the case expression to rPidVar.
            ErlMatch assignRPid = new ErlMatch(rPidVar, caseExpr);
            bodySeq.addExpression(assignRPid);

            // Send a message to that role:
            // Build the message tuple: {<self>_pid, self()}
            String selfField = self.toString().toLowerCase() + "_pid";
            ErlTuple msgTuple = new ErlTuple(List.of(
                    new ErlAtom(selfField),
                    new ErlCall("self", Collections.emptyList())
            ));
            // Build the send expression: rPidVar ! {<self>_pid, self()}
            ErlCall sendExpr = new ErlCall(new ErlOp("!"),
                    List.of(rPidVar, msgTuple));
            bodySeq.addExpression(sendExpr);
        }

        // --- Create state data record.
        // Build a record update for state_data with:
        // - For each role (other than self) add a field <role>_pid bound to that role's PID variable.
        LinkedHashMap<String, ErlTerm> recFields = new LinkedHashMap<>();
        recFields.put("mc_counter_1", new ErlAtom("0")); // you could later compute this dynamically.
        for (Role r : roles) {
            if (r.equals(self))
                continue;
            String rName = r.toString().toLowerCase();
            recFields.put(rName + "_pid", new ErlVar(rName + "Pid"));
        }
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

        // Return tuple
        ErlTerm retTuple = genNextState(efsm, initState);
        bodySeq.addExpression(retTuple);

        // Create the init function.
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
                // 2. Second argument: a tuple {<Role>Pid, <paramA>}.
                ErlTerm arg2 = new ErlTuple(List.of(
                        new ErlVar(e.role.toString() + "Pid"),
                        new ErlAtom(paramA)
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
        //TODO: only generate make_choice_s if multiple options
        res.add(genMakeChoice_s(s));
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
                List<ErlTerm> headArgs = List.of(new ErlAtom("internal"), arg2, arg3);

                // Build the clause body as a sequence.
                ErlSeq bodySeq = new ErlSeq();
                // First expression: gen_role:send_<paramA>(<Role>Pid, paramA)
                ErlCall sendCall = new ErlCall(
                        new ErlAtom("gen_") + self.toString().toLowerCase(),
                        "send_" + paramA,
                        List.of(new ErlVar(a.role.toString() + "Pid"), new ErlAtom(paramA))
                );
                bodySeq.addExpression(sendCall);
                // Second expression: the next state expression.
                ErlTerm nextStateExpr = genNextState(m, y.right);
                bodySeq.addExpression(nextStateExpr);

                // Create a new ErlFun clause with the given head and body.
                ErlFun clause = new ErlFun(funName);
                clause.addClause(headArgs, bodySeq);
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

        // --- Process Tau transitions (send-star branch)
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
        ErlCase tauCase = new ErlCase(makeChoiceCall);


        // Clause 1: Pattern "1" -> next state expression.
        //TODO: genSendFun(paramA)
        tauCase.addClause(new ErlAtom("1"),
                new ErlTuple(Arrays.asList(new ErlAtom("keep_state"), new ErlVar("Data"))));
        // Clause 2: Pattern "2" -> send call then next state.
        ErlCall rhsSendCall = new ErlCall(new ErlAtom("gen_" + self.toString().toLowerCase()), "send_" + paramA,
                List.of(new ErlVar(a.role.toString() + "Pid"), new ErlVar("Data")));
        ErlSeq rhsBodySeq = new ErlSeq();
        rhsBodySeq.addExpression(rhsSendCall);
        rhsBodySeq.addExpression(genNextState(m, sendStar.right));
        tauCase.addClause(new ErlAtom("2"), rhsBodySeq);
        ErlFun rhsSendFun = genMakeChoice_a(a.op);

        // Create the tau clause with head and body.
        ErlFun tauClause = new ErlFun(funName);
        tauClause.addClause(tauHead, tauCase);
        res.add(rhsSendFun);
        res.add(tauClause);

        // --- Process external events (pattern ?a)
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

            // Build head: [ cast, {<Role>Pid, a1}, Data ]
            ErlTerm headCast = new ErlAtom("cast");
            ErlTuple headExtTuple = new ErlTuple(List.of(
                    new ErlVar(e.role.toString() + "Pid"),
                    new ErlAtom(a1)
            ));

            Map<String, ErlTerm> lhsFields = new LinkedHashMap<>();
            lhsFields.put(a.role.toString().toLowerCase() + "_pid", new ErlVar(a.role.toString() + "Pid"));
            ErlRecordPattern lhsRecPattern = new ErlRecordPattern("state_data", fields);
            ErlTerm lhsData = new ErlMatch(lhsRecPattern, new ErlVar("Data"));

            List<ErlTerm> extHead = List.of(headCast, headExtTuple, lhsData);

            // Generate the function for make_choice on external events.
            ErlFun sendFunc = genMakeChoice_a(e.op);

            // Build body as a case expression.
            // Call: make_choice_<a1>(Data)
            ErlCall extMakeChoiceCall = new ErlCall("make_choice_" + a1, List.of(new ErlVar("Data")));
            ErlCase extCase = new ErlCase(extMakeChoiceCall);
            // Clause 1: Pattern "1" -> next state expression.
            extCase.addClause(new ErlAtom("1"), genNextState(m, succ.right));
            // Clause 2: Pattern "2" -> send call then next state.
            ErlCall extSendCall = new ErlCall(new ErlAtom("gen_" + self.toString().toLowerCase()), "send_" + a1,
                    List.of(new ErlVar(e.role.toString() + "Pid"), new ErlVar("Data")));
            ErlSeq extBodySeq = new ErlSeq();
            extBodySeq.addExpression(extSendCall);
            extBodySeq.addExpression(genNextState(m, succ.right));
            extCase.addClause(new ErlAtom("2"), extBodySeq);

            ErlFun extClause = new ErlFun(funName);
            extClause.addClause(extHead, extCase);

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

        return makeChoiceFunction;
    }


    // !!! pay?  -- ! and !*
    //TODO: call from mixed_internal/select?
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

        return makeChoiceFunction;
    }

//    protected ErlTerm genNextState(GTEFSM m, GTVState succ) {
//        switch (GTGenUtil.getStateKind(m, succ)) {
//            case END:
//                return new ErlTuple(List.of(
//                        new ErlAtom("stop"),
//                        new ErlAtom("normal"),
//                        new ErlVar("Data")
//                ));
//            case SELECT:
//            case INTERNAL_MIXED:
//            case EXTERNAL_MIXED_OI: {
//                // Get all transitions from the successor state.
//                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
//                        GTGenUtil.filterEdgesByState(m, succ);
//                String sName = GTGenUtil.stateToFuncName(succ);
//                // Filter for transitions whose event is a GTVTau.
//                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> tauTransitions =
//                        filt.entrySet().stream()
//                                .filter(e -> e.getKey().right instanceof GTVTau)
//                                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
//                // If exactly one such transition exists generate a direct tuple.
//                if (tauTransitions.size() == 1) {
//                    Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> entry =
//                            tauTransitions.entrySet().iterator().next();
//                        return new ErlTuple(List.of(
//                                new ErlAtom("next_state"),
//                                new ErlAtom(sName),
//                                new ErlVar("Data")
//                        ));
//                }
//                // Otherwise, build a case expression.
//                ErlCall makeChoiceCall = new ErlCall("make_choice_" + sName, List.of(new ErlVar("Data")));
//                ErlCase caseExpr = new ErlCase(makeChoiceCall);
//                // For each Tau transition, add a clause.
//                tauTransitions.forEach((key, actions) -> {
//                    // For each action in this transition, add a clause.
//                    actions.forEach(pair -> {
//                        GTVTau tau = (GTVTau) key.right;
//                        String a = GTGenUtil.eventToParam(tau);
//                        // Clause pattern: simply an atom with the outcome.
//                        ErlAtom clausePattern = new ErlAtom(a);
//                        // Clause body: {next_state, sName, Data, [next_event, internal, {a}]}
//                        ErlTuple bodyTuple = new ErlTuple(List.of(
//                                new ErlAtom("next_state"),
//                                new ErlAtom(sName),
//                                new ErlVar("Data"),
//                                new ErlList(List.of(
//                                        new ErlTuple(List.of(
//                                                new ErlAtom("next_event"),
//                                                new ErlAtom("internal"),
//                                                new ErlTuple(List.of(new ErlAtom(a)))
//                                        ))
//                                ))
//                        ));
//                        caseExpr.addClause(clausePattern, bodyTuple);
//                    });
//                });
//                return caseExpr;
//            }
//            case BRANCH:
//            case EXTERNAL_MIXED_II:
//            case EXTERNAL_MIXED_NOT_ENTRY:
//                return new ErlTuple(List.of(
//                        new ErlAtom("next_state"),
//                        new ErlAtom(GTGenUtil.stateToFuncName(succ)),
//                        new ErlVar("Data")
//                ));
//        }
//        throw new RuntimeException("Shouldn't get here?");
//    }

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
