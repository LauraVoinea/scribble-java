package org.scribble.ext.gt.codegen.erlang;

import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.efsm.GTEFSM;
import org.scribble.ext.gt.core.model.efsm.GTVState;
import org.scribble.ext.gt.core.model.efsm.event.GTVAction;
import org.scribble.ext.gt.core.model.efsm.event.GTVEvent;
import org.scribble.ext.gt.core.model.efsm.event.GTVTau;
import org.scribble.util.Pair;

import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class GTErlGenUtil {
//    private static final Pattern SPEC_PATTERN = Pattern.compile("^([^\\(]+)\\((.*)\\)\\s*->\\s*(.+)$");
    private static final Pattern SPEC_PATTERN =
            Pattern.compile(
                    "^([^\\(]+)\\((.*)\\)\\s*->\\s*(.+?(?:\\|.+?)*)$",
                    Pattern.DOTALL
            );

    static String genStateDataType(GTEFSM efsm, Set<Role> roles) {
        // Generate counter fields for every state in the EFSM.
        Set<String> counterFields = efsm.S.stream()
                .filter(s -> s.c > 0)
                .map(s -> "mc_counter_" + s.c + " :: integer()")
                .collect(Collectors.toCollection(LinkedHashSet::new));

        // Create pid fields for each role.
        Set<String> rolePidFields = roles.stream()
                .map(role -> role.toString().toLowerCase() + "_pid :: pid() | undefined")
                .collect(Collectors.toCollection(LinkedHashSet::new));

        List<String> allFields = new ArrayList<>();
        allFields.addAll(counterFields);
        allFields.addAll(rolePidFields);

        return "-type state_data() :: #state_data{" + String.join(", ", allFields) + "}.";
    }

    protected static List<ErlFun> aggregateTypeSpecs(List<ErlFun> stateFunctions) {
        Map<String, List<ErlFun>> groupedStateFunctions = stateFunctions.stream()
                .collect(Collectors.groupingBy(ErlFun::getName));

        List<ErlFun> aggregatedList = new ArrayList<>();

        // Process each group of state functions with the same name.
        for (Map.Entry<String, List<ErlFun>> entry : groupedStateFunctions.entrySet()) {
            String funName = entry.getKey();
            ErlFun aggregated = new ErlFun(funName);
            // Combine clauses from all functions in the group, removing duplicates.
            Set<String> seenClauses = new HashSet<>();
            for (ErlFun clauseFun : entry.getValue()) {
                for (ErlFun.FunClause fc : clauseFun.getClauses()) {
                    String key = fc.args.toString() + "|" + (fc.guard != null ? fc.guard.toString() : "") + "|" + fc.body.toString();
                    if (seenClauses.add(key)) {
                        aggregated.addClause(fc.args, fc.guard, fc.body);
                    }
                }
            }
            // Aggregate and deduplicate specs from all functions in the group.
            List<String> specsList = entry.getValue().stream()
                    .map(ErlFun::getSpec)
                    .filter(spec -> spec != null && !spec.isEmpty())
                    .distinct()
                    .collect(Collectors.toList());
            String aggregatedSpec = "";
            if (!specsList.isEmpty()) {
                aggregatedSpec = aggregateSpec(specsList);
                if (aggregatedSpec.isEmpty()) {
                    // Fall back to first raw spec if aggregation failed
                    aggregatedSpec = specsList.get(0);
                }
            }
            if (!aggregatedSpec.isEmpty()) {
                aggregated.setSpec(aggregatedSpec);
            }
            aggregatedList.add(aggregated);
        }
        return aggregatedList;
    }


    /**
     * Aggregates and writes the state functions to the given writer.
     *
     * @param writer         The file writer to output the Erlang code.
     * @param stateFunctions The list of generated state functions.
     */
    protected static void writeStateFunctions(FileWriter writer, List<ErlFun> stateFunctions) throws IOException {

        for (ErlFun fun : stateFunctions) {
            // 1) pretty‐print the spec
            String rawSpec = fun.getSpec();
            if (rawSpec != null && !rawSpec.isEmpty()) {
                // split head and return‐type parts
                String[] parts = rawSpec.split("->", 2);
                String head = parts[0].trim();
                String body = (parts.length > 1 ? parts[1].trim() : "");
                // pretty-print spec with '|' at line ends and '.' after last alt
                String[] alts = body.split("\\|");
                if (alts.length > 1) {
                    // multi-line spec: newline after ->
                    writer.writeLine("-spec " + head + " ->");
                    for (int i = 0; i < alts.length; i++) {
                        String alt = alts[i].trim();
                        if (i < alts.length - 1) {
                            writer.writeLine("    " + alt + " |");
                        } else {
                            writer.writeLine("    " + alt + ".");
                        }
                    }
                } else if (alts.length == 1) {
                    // single alternative
                    String alt = alts[0].trim();
                    writer.writeLine("-spec " + head + " -> " + alt + ".");
                }
            }
            // 2) write just the clauses (skip auto‐spec)
            fun.setSpec(null);
            fun.write(writer);
            fun.setSpec(rawSpec);
            writer.writeLine("");
        }
    }


    private static List<String> splitParams(String paramStr) {
        List<String> params = new ArrayList<>();
        StringBuilder current = new StringBuilder();
        int depth = 0;
        for (int i = 0; i < paramStr.length(); i++) {
            char c = paramStr.charAt(i);
            if (c == '{') {
                depth++;
            } else if (c == '}') {
                depth--;
            }
            // Split on comma only if we're not inside a curly brace.
            if (c == ',' && depth == 0) {
                params.add(current.toString().trim());
                current.setLength(0);
            } else {
                current.append(c);
            }
        }
        if (!current.isEmpty()) {
            params.add(current.toString().trim());
        }
        return params;
    }

    public static String aggregateSpec(Collection<String> specs) {
        List<String> functionNames = new ArrayList<>();
        List<List<String>> paramLists = new ArrayList<>();
        List<String> rhsList = new ArrayList<>();


        for (String spec : specs) {
            Matcher matcher = SPEC_PATTERN.matcher(spec);
            if (!matcher.matches()) {
                continue;
            }
            String funName = matcher.group(1).trim();
            functionNames.add(funName);

            // Extract and split the parameters
            String paramsStr = matcher.group(2).trim();
            List<String> params = splitParams(paramsStr);
            paramLists.add(params);

            // Extract the return part.
            rhsList.add(matcher.group(3).trim());
        }

        if (paramLists.isEmpty()) {
            return "";
        }

        // Ensure that all specs have the same function name.
        String baseFunName = functionNames.get(0);
        for (String name : functionNames) {
            if (!name.equals(baseFunName)) {
                throw new IllegalArgumentException("Mismatched function names: " + name + " vs " + baseFunName);
            }
        }

        // Ensure that all parameter lists have the same number of parameters.
        int paramCount = paramLists.get(0).size();
        for (List<String> params : paramLists) {
            if (params.size() != paramCount) {
                throw new IllegalArgumentException("Inconsistent number of parameters in spec: " + params);
            }
        }

        // Aggregate parameters per position.
        List<String> aggregatedParams = IntStream.range(0, paramCount)
                .mapToObj(i -> {
                    Set<String> distinctParams = paramLists.stream()
                            .map(params -> params.get(i))
                            .collect(Collectors.toCollection(LinkedHashSet::new));
                    return distinctParams.size() == 1 ? distinctParams.iterator().next() :
                            String.join(" | ", distinctParams);
                })
                .collect(Collectors.toList());

        // Split individual RHS alternatives, then remove duplicates while preserving order.
        List<String> allAlts = rhsList.stream()
                .flatMap(r -> Arrays.stream(r.split("\\|")))
                .map(String::trim)
                .collect(Collectors.toList());
        List<String> distinctRhs = new ArrayList<>(new LinkedHashSet<>(allAlts));
        String aggregatedRHS = String.join(" | ", distinctRhs);

        // Build and return the unified aggregated spec.
        return baseFunName + "(" + String.join(", ", aggregatedParams) + ") -> " + aggregatedRHS;
    }

    protected static String getNextStateReturnType(GTEFSM m, GTVState succ) {
        switch (GTGenUtil.getStateKind(m, succ)) {
            case END:
                return "{stop, normal, state_data()}";
            case SELECT:
            case INTERNAL_MIXED:
            case EXTERNAL_MIXED_OI: {
                // Get all transitions from the successor state.
                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> filt =
                        GTGenUtil.filterEdgesByState(m, succ);
                String sName = GTGenUtil.stateToFuncName(succ);
                // Filter for transitions for which the event is a GTVTau.
                Map<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> tauTransitions =
                        filt.entrySet().stream()
                                .filter(e -> e.getKey().right instanceof GTVTau)
                                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
                if (tauTransitions.size() == 1) {
                    Map.Entry<Pair<GTVState, GTVEvent>, Set<Pair<GTVAction, GTVState>>> entry =
                            tauTransitions.entrySet().iterator().next();
                    GTVTau tau = (GTVTau) entry.getKey().right;
                    String a = GTGenUtil.eventToParam(tau);
                    if (succ.equals(m.init))
                        return "{ok, " + sName + ", state_data(), " +
                                "[{next_event, internal, {" + new ErlAtom(a) + "}}]}";
                    // Return a tuple with an extra list element.
                    return "{next_state, " + sName + ", state_data(), " +
                            "[{next_event, internal, {" + new ErlAtom(a) + "}}]}" +
                            " | \n\t {keep_state, state_data()}";
                } else {
                    if (succ.equals(m.init))
                        return "{ok, " + sName + ", state_data()} | {next_state, " + sName + ", state_data(), [term()]}";
                    // Return a union of possible return types.
                    StringBuilder returnType = new StringBuilder();
                    for (Pair<GTVState, GTVEvent> key : tauTransitions.keySet()) {
                        GTVTau tau = (GTVTau) key.right;
                        String a = GTGenUtil.eventToParam(tau);
                        if (tauTransitions.get(key).size() == 1) {
                            returnType.append("{next_state, ")
                                    .append(sName).append(", state_data(), ")
                                    .append("[{next_event, internal, {")
                                    .append(new ErlAtom(a)).append("}}]} | \n\t ");
                        }
                    }

                    returnType.append("{keep_state, state_data()}");
                    return returnType.toString();
                }
            }
            case BRANCH:
            case EXTERNAL_MIXED_II:
            case EXTERNAL_MIXED_NOT_ENTRY:
                if (succ.equals(m.init))
                    return "{ok, " + GTGenUtil.stateToFuncName(succ) + ", state_data()}";
            return "{next_state, " + GTGenUtil.stateToFuncName(succ) + ", state_data()}";
        }
        throw new RuntimeException("Unexpected state in next-state generation.");
    }


}
