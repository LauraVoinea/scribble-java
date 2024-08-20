package org.scribble.gt.codegen;

import org.scribble.ext.gt.core.model.local.GTLConfig;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

import org.scribble.core.type.name.*;
import org.scribble.ext.gt.core.type.session.local.*;

import java.util.*;


public class ErlangCodeGen {
    private static final String outputDir = "./generated";

    public static String lowercaseFirstLetter(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        char firstChar = Character.toLowerCase(str.charAt(0));
        if (str.length() == 1) {
            return String.valueOf(firstChar);
        }
        return firstChar + str.substring(1);
    }

    public static void genTerminal(StringBuilder erlRole, String indentation) {
        erlRole.append(indentation).append("io:format(\"Good bye!~n\"),\n");
        erlRole.append(indentation).append("% Stop the state machine with normal termination\n");
        erlRole.append(indentation).append("{stop, normal, {}}.\n");
    }
    public static void genErl(StateM fsm, StringBuilder erlRole, int indent) {
        String indentation = "\t".repeat(Math.max(0, indent)); // Indentation string

        Set<State> states = fsm.getStates();
        erlRole.append("-module('").append(fsm.getName()).append("').\n");
        erlRole.append(" % Define a macro SERVER that is replaced with ?MODULE (current module name)\n");
        erlRole.append("-define(SERVER, ?MODULE).\n");

        erlRole.append("-behaviour(gen_statem).\n");

        //TODO: list of exported funs instead
        erlRole.append("-compile(export_all).\n");
        erlRole.append("-compile(nowarn_export_all).\n\n");

        erlRole.append("% Function to start the state machine\n");
        erlRole.append("start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).\n\n");
        erlRole.append("callback_mode() -> [state_functions].\n\n");
//        erlRole.append(String.format("init([]) -> {ok, %s, {}}.\n\n", states.iterator().next().getName()));

        //State transition functions
        for (Transition t : fsm.getTransitions()){
            if(t.getEvent().getKind().equals(EventKind.SEND) && !t.getEvent().getRole().equals(fsm.getName())){
                erlRole.append(String.format("send_%s(%sPid) -> \n gen_statem:cast(%sPid, {%s}).\n \n",
                        t.getEvent().getName(), t.getEvent().getRole(), t.getEvent().getRole(), t.getEvent().getName()));
            }
        }

        //State functions
        for (State state : states) {
            // create init state
            String stateName = state.getName();
            StateKind stateKind = state.getKind();
            switch (stateKind) {
                case INIT:
                    erlRole.append("init([]) -> \n");
                    for (Transition t : state.getTransitions()) {
                        State nextState = t.getNextState();
                        if (nextState.getKind().equals(StateKind.TERMINAL)) {
                            genTerminal(erlRole, indentation);
                        } else if (nextState.getKind().equals(StateKind.INTERNAL)){
                            erlRole.append(indentation).append(String.format("{next_state, %s, {}, " +
                                            "[{next_event, internal, {send_%s}}]}.\n",
                                   nextState.getName(), t.getEvent().getName()));
                        } else {
                            erlRole.append(indentation).append(String.format("{next_state, %s, {}}.\n", nextState.getName()));
                        }
                    }
                    break;
                case INTERNAL:
                    for (Transition t : state.getTransitions()) {
                        State nextState = t.getNextState();
                        if(t.getEvent().getRole() != fsm.getName()){
                            erlRole.append(String.format("%s(internal, {%s}, {%sPid, Data}) -> \n",
                                    stateName, t.getEvent().getName(), t.getEvent().getRole()));
                            erlRole.append(indentation).append(String.format("send_%s(%sPid), \n",
                                    t.getEvent().getName(), t.getEvent().getRole()));
                        } else {
                            erlRole.append(String.format("%s(send, {%s}, Data) -> \n",
                                    stateName, t.getEvent().getName()));
                        }

                        if (nextState.getKind().equals(StateKind.TERMINAL)) {
                            genTerminal(erlRole, indentation);
                        } else if (nextState.getKind().equals(StateKind.INTERNAL)){
                            erlRole.append(String.format("{next_state, %s, Data, " +
                                            "[{next_event, internal, {send_%s}}]};\n",
                                     t.getEvent().getName(), t.getEvent().getName()));
                        } else {
                            erlRole.append(String.format("{next_state, %s, Data};\n",
                                     nextState.getName()));
                        }
                    }
                    erlRole.replace(erlRole.length() - 2, erlRole.length(), ".\n");
                    break;
                case EXTERNAL:
                    for (Transition t : state.getTransitions()) {
                        genExternal(erlRole, indentation, stateName, t);
                    }
                    erlRole.replace(erlRole.length() - 2, erlRole.length(), ".\n");
                    break;
                case MIXED_INTERNAL:
                    List<Transition> transitions = new ArrayList<>(state.getTransitions());

                    Transition rhs = transitions.get(transitions.size() - 1);
                    State next = rhs.getNextState();
                    if(!rhs.getEvent().getRole().equals(fsm.getName())){
                        erlRole.append(String.format("%s(internal, {%s}, {%sPid, Data}) -> \n",
                                stateName, rhs.getEvent().getName(), rhs.getEvent().getRole()));
                        erlRole.append(indentation).append(String.format("send_%s(%sPid), \n",
                                rhs.getEvent().getName(), rhs.getEvent().getRole()));
                    } else {
                        erlRole.append(String.format("%s(send, {%s}, Data) -> \n",
                                stateName, rhs.getEvent().getName()));
                    }

                    if (next.getKind().equals(StateKind.TERMINAL)) {
                        genTerminal(erlRole, indentation);
                    } else if (next.getKind().equals(StateKind.INTERNAL)){
                        erlRole.append(String.format("{next_state, %s, Data, " +
                                        "[{next_event, internal, {send_%s}}]};\n",
                                rhs.getEvent().getName(), rhs.getEvent().getName()));
                    } else {
                        erlRole.append(String.format("{next_state, %s, Data};\n",
                                next.getName()));
                    }

                    for (int i = 0 ; i < transitions.size() - 1; i++) {
                        Transition t = transitions.get(i);
                        genExternal(erlRole, indentation, stateName, t);
                    }

                    erlRole.replace(erlRole.length() - 2, erlRole.length(), ".\n");
                    break;
                case MIXED_EXTERNAL:
                case REC:
                    for (Transition t : state.getTransitions()) {
                        State nextState = t.getNextState();
                        if (nextState.getKind().equals(StateKind.TERMINAL))
                            erlRole.append(String.format("%s(cast, {%s}, Data) -> {stop, normal, Data};\n",
                                    stateName, t.getEvent().getName()));
                        else
                            erlRole.append(String.format("%s(cast, {%s}, Data) -> {next_state, %s, Data};\n",
                                    stateName, t.getEvent().getName(), t.getNextState().getName()));
                    }
                    erlRole.replace(erlRole.length() - 2, erlRole.length(), ".\n");
                    break;
                case TERMINAL:
                default:
                    erlRole.append("\n");
                    break;

            }
            erlRole.append("\n");
        }
    }

    private static void genExternal(StringBuilder erlRole, String indentation, String stateName, Transition t) {
        State nextState = t.getNextState();
        erlRole.append(String.format("%s(cast, {%s}, Data) -> \n",
                stateName, t.getEvent().getName()));
        if (nextState.getKind().equals(StateKind.TERMINAL)) {
            genTerminal(erlRole, indentation);
        } else {
            erlRole.append(String.format("{next_state, %s, Data};\n",
                    nextState.getName()));
        }
    }


    public static void genModule(String protocolName, Map<Role, GTLConfig> locals, Set<Op> committing) {
        Iterator<Map.Entry<Role, GTLConfig>> iterator = locals.entrySet().iterator();
        System.err.println("Protocol name" + protocolName);

        while(iterator.hasNext()){
            Map.Entry<Role, GTLConfig> aux = iterator.next();
            String role = String.valueOf(aux.getKey());
            StringBuilder erlRole = new StringBuilder();
            GTLConfig A = aux.getValue();
            GTLType type = A.type;

            StateM stateM = StateM.translate(type, aux.getKey(), committing, null);
            stateM.stateRenaming();
            System.out.println("StateM: " + stateM);
            stateM.generateDOT(outputDir + File.separator + protocolName , role + "_fsm.dot");
            genErl(stateM, erlRole, 1);
            createErlangFile(protocolName, role, erlRole);

        }
    }

    private static void createErlangFile(String protocolName, String moduleName, StringBuilder erlRole) {
        // Ensure the output directory exists
        String outputDirectory = outputDir + File.separator + protocolName;
        File directory = new File(outputDirectory);
        if (!directory.exists()) {
            directory.mkdirs();
        }

        // Create a FileWriter for the Erlang file
        try (FileWriter fileWriter = new FileWriter(outputDirectory + File.separator + moduleName + ".erl")) {
            fileWriter.write(String.valueOf(erlRole));
            fileWriter.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }



}
