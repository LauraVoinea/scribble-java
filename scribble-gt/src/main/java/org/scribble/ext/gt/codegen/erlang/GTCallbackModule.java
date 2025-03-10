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
        exportNames.add("start_link/2");

        // Prepare a list to store all generated state functions
        List<ErlFun> stateFunctions = new ArrayList<>();
        List<ErlFun> membs = new LinkedList<>();

//        for (GTVState s : m.S) {
//            switch (GTGenUtil.getStateKind(m, s)) {
//                case END -> { }
//                case BRANCH -> membs.addAll(generateBranch(m, s));
//                case SELECT -> membs.addAll(generateSelect(m, s));
//                case INTERNAL_MIXED -> membs.addAll(generateInternalMixed(m, s));
//                case EXTERNAL_MIXED_OI -> membs.addAll(generateExternalMixedOI(m, s));
//                case EXTERNAL_MIXED_II -> membs.addAll(generateExternalMixedII(m, s));
//                case EXTERNAL_MIXED_NOT_ENTRY -> membs.addAll(generateExternalMixedNotEntry(m, s));
//            }
//        }

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


    protected List<ErlFun> generateBranch(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateBranchAux(m, s, filt);
    }



//    protected List<ErlFun> generateBranchAux(
//            GTEFSM m, GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
//        return edges.entrySet().stream().flatMap(x -> {
//            Pair<GTVState, GTVEvent> k = x.getKey();
//            GTVRecv e = (GTVRecv) k.right;
//            String name = GTGenUtil.stateToFuncName(s);
//            String param_a = GTGenUtil.eventToParam(e);
//            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
//            return vs.stream().map(y -> {
//                List<String> params = List.of("cast", "{" + e.role + "Pid, " + param_a + "}", "Data");
//                String body = genNextState(m, y.right);
//                return new ErlFun(name, params, body);
//            });
//        }).collect(Collectors.toList());
//    }

    protected List<ErlFun> generateBranchAux(
            GTEFSM m, GTVState s,
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
        return edges.entrySet().stream().flatMap(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            // Expecting a receive event
            GTVRecv e = (GTVRecv) key.right;
            String funcName = GTGenUtil.stateToFuncName(s);
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
                // TODO: Add additional fields (e.g. PID fields) if necessary.
                fields.put("mc_counter_" + s.c, new ErlVar("MC"));
                ErlRecordPattern recPattern = new ErlRecordPattern("state_data", fields);
                ErlTerm arg3 = new ErlMatch(recPattern, new ErlVar("Data"));
                List<ErlTerm> headArgs = List.of(arg1, arg2, arg3);

                // Build the clause body.
                // For now, we wrap the raw next-state expression in an ErlAtom.
                ErlTerm body = new ErlAtom(genNextState(m, pair.right));

                // Create a new function clause with the given head and body.
                ErlFun clause = new ErlFun(funcName);
                clause.addClause(headArgs, body);
                return clause;
            });
        }).collect(Collectors.toList());
    }


    protected List<ErlFun> generateSelect(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        return generateSelectAux(m, s, filt);
    }

//    protected List<ErlFun> generateSelectAux(
//            GTEFSM m, GTVState s, Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
//        List<ErlFun> res = new LinkedList<>();
//        //res.add(genMakeChoice_s(s));
//        res.addAll(edges.entrySet().stream().flatMap(x -> {
//            //Pair<GTVState, GTVEvent> k = x.getKey();  // tau_a
//            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
//            return vs.stream().map(y -> {
//                GTVSend a = (GTVSend) y.left;
//                //Role r = (a instanceof GTVSend) ? ((GTVSend) a).role : ((GTVSendStar) a).role;
//                String name = GTGenUtil.stateToFuncName(s);
//                String param_a = GTGenUtil.sendToParam(a);
//                List<String> params = List.of("internal", "{" + param_a + "}", "Data");
//                String body = "gen_role:send_" + param_a + "(" + a.role + "Pid, " + param_a + "),\n"
//                        + genNextState(m, y.right);
//                return new ErlFun(name, params, body);
//            });
//        }).collect(Collectors.toList()));
//        return res;
//    }

    protected List<ErlFun> generateSelectAux(
            GTEFSM m, GTVState s,
            Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> edges) {
        List<ErlFun> res = new LinkedList<>();
        // Optionally, you might add a make_choice clause here:
        // res.add(genMakeChoice_s(s));
        res.addAll(edges.entrySet().stream().flatMap(entry -> {
            Set<Pair<GTVAction, GTVState>> actions = entry.getValue();
            return actions.stream().map(y -> {
                // For each send action, assume it is a GTVSend.
                GTVSend a = (GTVSend) y.left;
                String funcName = GTGenUtil.stateToFuncName(s);
                String paramA = GTGenUtil.sendToParam(a);

                ErlTuple tupleParam = new ErlTuple(List.of(new ErlAtom(paramA)));
                ErlVar dataVar = new ErlVar("Data");
                List<ErlTerm> headArgs = List.of( new ErlAtom("internal"), tupleParam, dataVar);

                // Build the clause body as a sequence.
                ErlSeq bodySeq = new ErlSeq();
                // First expression: gen_role:send_<paramA>(<Role>Pid, paramA)
                ErlCall sendCall = new ErlCall(
                        new ErlAtom("gen_role"),
                        "send_" + paramA,
                        List.of(new ErlVar(a.role.toString() + "Pid"), new ErlAtom(paramA))
                );
                bodySeq.addExpression(sendCall);
                // Second expression: the next state expression.
                ErlAtom nextStateExpr = new ErlAtom(genNextState(m, y.right));
                bodySeq.addExpression(nextStateExpr);

                // Create a new ErlFun clause with the given head and body.
                ErlFun clause = new ErlFun(funcName);
                clause.addClause(headArgs, bodySeq);
                return clause;
            });
        }).collect(Collectors.toList()));
        return res;
    }



//    protected List<ErlFun> generateInternalMixed(GTEFSM m, GTVState s) {
//        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
//
//        String name = GTGenUtil.stateToFuncName(s);
//        List<ErlFun> res = new LinkedList<>();
//
//        //res.add(genMakeChoice_s(s));
//
//        // !!! TODO missing ?/!* case
//        /*Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
//                filt.entrySet().stream().filter(x ->
//                        x.getValue().stream().anyMatch(y -> y.left instanceof GTVSendStar)).collect(
//                        Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (x, y) -> null, LinkedHashMap::new));*/
//        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> rhs =
//                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
//        if (rhs.size() != 1) {
//            throw new RuntimeException("Shouldn't get here? " + rhs);
//        }
//        Set<Pair<GTVAction, GTVState>> sendStars = rhs.values().iterator().next();
//        if (sendStars.size() != 1) {
//            throw new RuntimeException("Shouldn't get here? " + rhs);
//        }
//
//        Pair<GTVAction, GTVState> sendStar = sendStars.iterator().next();
//        GTVSendStar a = (GTVSendStar) sendStar.left;
//        String param_a = GTGenUtil.sendToParam(a);
//        List<String> params = List.of("internal", "{" + param_a + "}", "Data");
//        String body = "case make_choice_" + param_a + "(" + a.role + "Pid, Data),\n"
//                + genNextState(m, sendStar.right);
//        res.add(new ErlFun(name, params, body));
//
//        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
//                GTGenUtil.filterEdgesByEvent(filt, x -> x.getKind() == GTVEvent.Kind.EXTERNAL);
//        res.addAll(lhs.entrySet().stream().map(x -> {
//            Pair<GTVState, GTVEvent> k = x.getKey();
//            Set<Pair<GTVAction, GTVState>> vs = x.getValue();
//            GTVRecv e = (GTVRecv) k.right;
//            if (vs.size() != 1 && vs.stream().filter(y -> y.left instanceof GTVEpsilon).count() != 1) {
//                throw new RuntimeException("Shouldn't get here: " + k + " ,, " + vs);
//            }
//            Pair<GTVAction, GTVState> succ = vs.iterator().next();
//            String a1 = GTGenUtil.eventToParam(e);  // !!! pay?
//            List<String> ps = List.of("cast", "{" + e.role + "Pid, " + a1 + ", Data");
//            String next = genNextState(m, succ.right);
//            String b = "case make_choice_" + a1 + "(Data) of\n"
//                    + "1 -> " + next + "\n"
//                    + "2 -> gen_role:send_" + a1 + "(" + e.role + "Pid, Data),\n"
//                    + next;
//            return new ErlFun(name, ps, b);
//        }).collect(Collectors.toList()));
//
//        return res;
//    }

    protected List<ErlFun> generateInternalMixed(GTEFSM m, GTVState s) {
        // Get the transitions for state s.
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                GTGenUtil.filterEdgesByState(m, s);
        String funcName = GTGenUtil.stateToFuncName(s);
        List<ErlFun> res = new LinkedList<>();

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
        ErlTuple tupleParam = new ErlTuple(List.of(new ErlAtom(paramA)));
        ErlTerm dataVar = new ErlVar("Data");
        List<ErlTerm> tauHead = List.of( new ErlAtom("internal"), tupleParam, dataVar);

        // Build body:
        // Construct a case expression as a raw ErlAtom for now.
        // It will appear as:
        // "case make_choice_<paramA>(<Role>Pid, Data) of\n<next-state-expression>"
        String casePrefix = "case make_choice_" + paramA + "(" + a.role.toString() + "Pid, Data) of\n";
        String nextExpr = genNextState(m, sendStar.right);
        ErlTerm tauBody = new ErlAtom(casePrefix + nextExpr);

        ErlFun tauClause = new ErlFun(funcName);
        tauClause.addClause(tauHead, tauBody);
        res.add(tauClause);

        // --- Process external events
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> extMap =
                GTGenUtil.filterEdgesByEvent(filt, x -> x.getKind() == GTVEvent.Kind.EXTERNAL);
        List<ErlFun> externalClauses = extMap.entrySet().stream().map(entry -> {
            Pair<GTVState, GTVEvent> key = entry.getKey();
            Set<Pair<GTVAction, GTVState>> vs = entry.getValue();
            GTVRecv e = (GTVRecv) key.right;
            // For simplicity, assume one clause per edge.
            if (vs.size() != 1 && vs.stream().filter(y -> y.left instanceof GTVEpsilon).count() != 1) {
                throw new RuntimeException("Unexpected external branch clause: " + key + " , " + vs);
            }
            Pair<GTVAction, GTVState> succ = vs.iterator().next();
            String a1 = GTGenUtil.eventToParam(e);

            // Build head: [ cast, {<Role>Pid, a1, Counter}, Data ]
            ErlVar rolePid = new ErlVar(e.role.toString() + "Pid");
            ErlTuple headTuple = new ErlTuple(List.of(rolePid, new ErlAtom(a1), new ErlVar("Counter")));
            List<ErlTerm> extHead = List.of( new ErlAtom("cast"), headTuple, new ErlVar("Data"));

            // Build body as a case expression.
            // For example:
            // "case make_choice_<a1>(Data) of\n    1 -> <next> \n    2 -> gen_role:send_<a1>(<Role>Pid, Data),\n<next>"
            String next = genNextState(m, succ.right);
            String bodyStr = "case make_choice_" + a1 + "(Data) of\n" +
                    "    1 -> " + next + "\n" +
                    "    2 -> gen_role:send_" + a1 + "(" + e.role.toString() + "Pid, Data),\n" +
                    next;
            ErlTerm extBody = new ErlAtom(bodyStr);
            ErlFun extClause = new ErlFun(funcName);
            extClause.addClause(extHead, extBody);
            return extClause;
        }).collect(Collectors.toList());
        res.addAll(externalClauses);

        return res;
    }

    protected List<ErlFun> generateExternalMixedOI(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(m, s, lhs));

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

    protected List<ErlFun> generateExternalMixedNotEntry(GTEFSM m, GTVState s) {
        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt = GTGenUtil.filterEdgesByState(m, s);
        List<ErlFun> res = new LinkedList<>();

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs =
                GTGenUtil.filterEdgesByAnyAction(filt, x -> x instanceof GTVEpsilon);
        res.addAll(generateBranchAux(m, s, lhs));

        Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> lhs_tau =  // !!! -- ! |> ?
                GTGenUtil.filterEdgesByEvent(filt, x -> x instanceof GTVTau);
        res.addAll(generateSelectAux(m, s, lhs_tau));

        return res;
    }

    // !!! move to gen_role
    protected ErlFun genMakeChoice_s(GTVState s) {
        String name = "make_choice_" + GTGenUtil.stateToFuncName(s);
        List<String> params = List.of("Data");
        String body = "rand:uniform(2)";
        return new ErlFun(name);
    }

    // !!! move to gen_role
    // !!! pay?  -- ! and !*
    protected ErlFun genMakeChoice_a(Op op) {
        String name = "make_choice_" + op;
        List<String> params = List.of("Data");
        String body = "rand:uniform(2)";
        return new ErlFun(name);
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
                        "case make_choice_" + s + "(Data) of\n"
                                + filt.keySet().stream().filter(x -> x.right instanceof GTVTau).map(x -> {
                            GTVTau tau = (GTVTau) x.right;
                            String a = GTGenUtil.eventToParam(tau);  // !!! pay?
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
