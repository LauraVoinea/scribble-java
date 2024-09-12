package org.scribble.gt.codegen;

import java.util.Set;

import static org.scribble.gt.codegen.CodeGenUtils.*;

public class CallbackModule {
    /**
     * Generates the Erlang code for the callback module that implements protocol-specific logic.
     */
    public static void genCallbackModule(String role, StringBuilder erlCode, StateM fsm) {
        String lowercaseRole = lowercaseFirstLetter(role);
        erlCode.append("-module(").append(lowercaseRole).append(").\n");
        erlCode.append(String.format("-behaviour(gen_%s).\n\n", lowercaseRole));

        //TODO: break the export into multiple lines based on the number of functions
        erlCode.append("-export([start_link/0, init/1, ");
        fsm.getStates().stream().filter(state -> !state.getKind().equals(StateKind.TERMINAL) &&
                !state.getKind().equals(StateKind.INIT)).forEach(state -> {
            erlCode.append(state.getName()).append("/3, ");
        });
        erlCode.replace(erlCode.length() - 2, erlCode.length(), "]).\n\n");

        erlCode.append("-include(\"").append(lowercaseRole).append(".hrl\").\n\n");

        genStartLinkFunction(erlCode, lowercaseRole);
        genStateFunctions(erlCode, fsm, role);
    }

    private static void genStartLinkFunction(StringBuilder erlCode, String role) {
        erlCode.append("-spec start_link() -> {ok, pid()} | {error, any()}.\n");
        erlCode.append("start_link() ->\n");
        erlCode.append(String.format("  gen_%s:start_link(?MODULE, []).\n\n", role));
    }

    /**
     * Generates state functions for the callback module.
     */
    private static void genStateFunctions(StringBuilder erlCode, StateM fsm, String role) {
        // Generate protocol-specific logic for each state
        Set<State> states = fsm.getStates();
        for (State state : states) {
            switch (state.getKind()){
                case MIXED_INTERNAL:

                case MIXED_EXTERNAL:

                case REC:
                case EXTERNAL:

                case INTERNAL:
                    genInternalStateFunction(erlCode, state, role);
                    break;
                case INIT:
                    genInitStateFunction(erlCode, state);
                    break;
                case TERMINAL:
                default:
                    erlCode.append("\n");
                    break;
            }
        }



    }

    private static void genMixedInternalStateFunction(StringBuilder erlCode, State state) {
    }

    private static void genMixedExternalStateFunction(StringBuilder erlCode, State state) {
    }

    private static void genExternalStateFunction(StringBuilder erlCode, State state) {
    }

//    private static void genInternalStateFunction(StringBuilder erlCode, State state, String role) {
//        erlCode.append(String.format("-spec %s(atom(), ", state.getName()));
//        int index = erlCode.length();
//        StringBuilder resultTypes = new StringBuilder();
//        StringBuilder messageTypes = new StringBuilder();
//        for(Transition t : state.getTransitions()){
//            String event = t.getEvent().getName();
//            State nextState = t.getNextState();
//            String dst = t.getEvent().getRole().toString();
//            if (t.getEvent().getKind().equals(EventKind.MIXED_SEND)){
//                messageTypes.append(String.format(" %s_choice", event));
//                erlCode.append(String.format("%s(internal, %s_choice, #state_data{%s_pid = %sPid} = Data) ->\n",
//                        state.getName(), event, lowercaseFirstLetter(dst), dst));
//                // generic code to make a choice and then send the message or wait for the message
//                erlCode.append(indent(1)).append("io:format(\"Making a choice~n\"),\n");
//                erlCode.append(indent(1)).append("Choice = random:uniform(2),\n");
//                erlCode.append(indent(1)).append("case Choice of\n");
//                erlCode.append(indent(2)).append("1 ->\n");
//                erlCode.append(indent(3)).append("{keep_state, Data};\n");
//                erlCode.append(indent(2)).append("2 ->\n");
//                erlCode.append(indent(3)).append("io:format(\"Sending ~p~n\", [").append(event).append("]),\n");
//                erlCode.append(indent(3)).append(String.format("gen_%s:send_%s(%sPid),\n",
//                        role, event, dst));
//                erlCode.append(indent(2)).append("end;\n");
//                //TODO: fix the next state for this case; likely split into methods
//            } else if(t.getEvent().getKind().equals(EventKind.SEND)){
//                messageTypes.append(String.format("{%s}", event));
//                erlCode.append(String.format("%s(internal, {%s}, #state_data{%s_pid = %sPid} = Data) ->\n",
//                        state.getName(), event, lowercaseFirstLetter(dst), dst));
//                erlCode.append(indent(1)).append("io:format(\"Sending ~p~n\", [").append(event).append("]),\n");
//                erlCode.append(indent(1)).
//                        append(String.format("gen_%s:send_%s(%sPid),\n",
//                                role, event, dst));
//            }else{
//                messageTypes.append(String.format("{pid(), {%s}}", event));
//                erlCode.append(String.format("%s(cast, {%sPid, {%s}}, #state_data{%s_pid = %sPid} = Data) ->\n",
//                        state.getName(), t.getEvent().getRole(), event, lowercaseFirstLetter(dst), dst));
//            }
//            messageTypes.append(" | ");
//
//            if(nextState.getKind().equals(StateKind.TERMINAL)){
//                resultTypes.append("{stop, normal, state_data()}");
//            } else {
//                resultTypes.append(String.format("{next_state, %s, state_data()}", nextState.getName()));
//            }
//            resultTypes.append(" | ");
//
//            if(nextState.getKind().equals(StateKind.TERMINAL)){
//                erlCode.append(indent(1)).append("{stop, normal, Data};\n");
//            } else {
//                erlCode.append(indent(1)).
//                        append(String.format("{next_state, %s, Data",
//                                nextState.getName()));
//                if(nextState.getKind().equals(StateKind.INTERNAL)){
//                    Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
//                    erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName()));
//                } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
//                    // get the last transition in the next state and add it as the next event
//                    //TODO better way to get rhs of mixed choice
//                    Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
//                    erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName()));
//                }
//                erlCode.append("};\n");
//
//            }
//        }
//
//        erlCode.append(String.format("%s(_EventType, _Msg, _Data) ->\n", state.getName()));
//        erlCode.append(indent(1)).append("io:format(\"Unexpected event in ").append(state.getName()).append(", ignoring~n\"),\n");
//        erlCode.append(indent(1)).append("{keep_state, _Data}.\n\n");
////        erlCode.replace(erlCode.length() - 2, erlCode.length(), ".\n\n");
//        erlCode.insert(index, messageTypes.substring(0, messageTypes.length() - 3)
//                + ", state_data()) -> " + resultTypes.substring(0, resultTypes.length() - 3) + ".\n");
//
//    }
private static void genInternalStateFunction(StringBuilder erlCode, State state, String role) {
    erlCode.append(String.format("-spec %s(atom(), ", state.getName()));
    int index = erlCode.length();

    StringBuilder resultTypes = new StringBuilder();
    StringBuilder messageTypes = new StringBuilder();

    for (Transition t : state.getTransitions()) {
        handleTransition(erlCode, t, state, role, resultTypes, messageTypes);
    }

    addDefaultCase(erlCode, state);
    appendSpecSignature(erlCode, index, messageTypes, resultTypes);
}

    private static void handleTransition(StringBuilder erlCode, Transition t, State state, String role,
                                         StringBuilder resultTypes, StringBuilder messageTypes) {
        String event = t.getEvent().getName();
        String dst = t.getEvent().getRole().toString();

        switch (t.getEvent().getKind()) {
            case MIXED_SEND:
                handleMixedSendTransition(erlCode, t, state, role, resultTypes);
                messageTypes.append(String.format(" %s_choice | ", event));
                break;
            case SEND:
                handleSendTransition(erlCode, t, state, role, resultTypes);
                messageTypes.append(String.format("{%s} | ", event));
                break;
            default:
                handleReceiveTransition(erlCode, event, dst, state, resultTypes, messageTypes);
                break;
        }
    }

    private static void handleMixedSendTransition(StringBuilder erlCode,Transition t, State state,
                                                  String role, StringBuilder resultTypes) {
        String event = t.getEvent().getName();
        String dst = t.getEvent().getRole().toString();
        erlCode.append(String.format("%s(internal, %s_choice, #state_data{%s_pid = %sPid} = Data) ->\n",
                state.getName(), event, lowercaseFirstLetter(dst), dst));
        erlCode.append(indent(1)).append("io:format(\"Making a choice~n\"),\n");
        erlCode.append(indent(1)).append("Choice = random:uniform(2),\n");
        erlCode.append(indent(1)).append("case Choice of\n");
        erlCode.append(indent(2)).append("1 ->\n");
        erlCode.append(indent(3)).append("{keep_state, Data};\n");
        erlCode.append(indent(2)).append("2 ->\n");
        erlCode.append(indent(3)).append("io:format(\"Sending ~p~n\", [").append(event).append("]),\n");
        erlCode.append(indent(3)).append(String.format("gen_%s:send_%s(%sPid),\n", role, event, dst));

        State nextState = t.getNextState();
        if(nextState.getKind().equals(StateKind.TERMINAL)){
            erlCode.append(indent(3)).append("{stop, normal, Data}\n");
        } else {
            erlCode.append(indent(3)).
                    append(String.format("{next_state, %s, Data",
                            nextState.getName()));
            if(nextState.getKind().equals(StateKind.INTERNAL)){
                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
                erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName()));
            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
                // get the last transition in the next state and add it as the next event
                //TODO better way to get rhs of mixed choice
                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName()));
            }
            erlCode.append("}\n");

        }
        erlCode.append(indent(2)).append("end;\n");

        resultTypes.append("{keep_state, state_data()} | \n").append(indent(1)).append("{next_state, ").
                append(state.getName()).append(", state_data()} | \n").append(indent(1));
    }

    private static void handleSendTransition(StringBuilder erlCode, Transition t, State state, String role,
                                             StringBuilder resultTypes) {
        String event = t.getEvent().getName();
        String dst = t.getEvent().getRole().toString();
        State nextState = t.getNextState();
        erlCode.append(String.format("%s(internal, {%s}, #state_data{%s_pid = %sPid} = Data) ->\n", state.getName(),
                event, lowercaseFirstLetter(dst), dst));
        erlCode.append(indent(1)).append("io:format(\"Sending ~p~n\", [").append(event).append("]),\n");
        erlCode.append(indent(1)).append(String.format("gen_%s:send_%s(%sPid),\n", role, event, dst));

        if(nextState.getKind().equals(StateKind.TERMINAL)){
            resultTypes.append("{stop, normal, state_data()} | \n").append(indent(1));
            erlCode.append(indent(1)).append("{stop, normal, Data};\n");
        } else {
            resultTypes.append(String.format("{next_state, %s, state_data()} | \n", state.getName())).append(indent(1));
            erlCode.append(indent(1)).
                    append(String.format("{next_state, %s, Data",
                            nextState.getName()));
            if(nextState.getKind().equals(StateKind.INTERNAL)){
                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
                erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName()));
            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
                // get the last transition in the next state and add it as the next event
                //TODO better way to get rhs of mixed choice
                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName()));
            }
            erlCode.append("};\n");

        }
    }

    private static void handleReceiveTransition(StringBuilder erlCode, String event, String dst, State state,
                                                StringBuilder resultTypes, StringBuilder messageTypes) {
        erlCode.append(String.format("%s(cast, {%sPid, {%s}}, #state_data{%s_pid = %sPid} = Data) ->\n",
                state.getName(), dst, event, lowercaseFirstLetter(dst), dst));
        messageTypes.append(String.format("{pid(), {%s}} | " , event));

        if (state.getKind().equals(StateKind.TERMINAL)) {
            erlCode.append(indent(1)).append("{stop, normal, Data};\n");
            resultTypes.append("{stop, normal, state_data()} | \n").append(indent(1));
        } else {
            erlCode.append(indent(1)).append(String.format("{next_state, %s, Data", state.getName())).append("};\n");
            resultTypes.append(String.format("{next_state, %s, state_data()} | \n", state.getName())).append(indent(1));
        }
    }

    private static void addDefaultCase(StringBuilder erlCode, State state) {
        erlCode.append(String.format("%s(_EventType, _Msg, _Data) ->\n", state.getName()));
        erlCode.append(indent(1)).append("io:format(\"Unexpected event in ").
                append(state.getName()).append(", ignoring~n\"),\n");
        erlCode.append(indent(1)).append("{keep_state, _Data}.\n\n");
    }

    private static void appendSpecSignature(StringBuilder erlCode, int index, StringBuilder messageTypes,
                                            StringBuilder resultTypes) {
        erlCode.insert(index, messageTypes.substring(0, messageTypes.length() - 3) + ", state_data()) -> " +
                resultTypes.substring(0, resultTypes.length() - 7) + ".\n");
    }

    private static void genInitStateFunction(StringBuilder erlCode, State state) {
        State nextState = state.getTransitions().stream().findFirst().get().getNextState();
        //TODO: if nextState after init is internal a new state needs to be added.
        if(nextState.getKind().equals(StateKind.TERMINAL)){
            erlCode.append("-spec init(list()) -> {ok, _}.\n");
            erlCode.append("init([]) ->\n");
            erlCode.append(indent(1)).append(String.format("{ok, %s}.\n\n", nextState.getName()));
        } else {
            erlCode.append(String.format("-spec init(list()) -> {ok, %s, state_data()}.\n", nextState.getName()));
            erlCode.append("init([]) ->\n");
            erlCode.append(indent(1)).append(String.format("{ok, %s, #state_data{}}.\n\n", nextState.getName()));
        }
    }


}
