package org.scribble.gt.codegen;

import org.scribble.core.type.name.Role;
import org.scribble.ext.gt.core.model.global.Theta;
import org.scribble.ext.gt.core.model.local.Sigma;

import java.util.Set;

import static org.scribble.gt.codegen.CodeGenUtils.*;
import static org.scribble.gt.codegen.ErlangCodeGen.lowercaseFirstLetter;

public class CallbackModule {
    /**
     * Generates the Erlang code for the callback module that implements protocol-specific logic.
     */
    public static void genCallbackModule(String role, StringBuilder erlCode, StateM fsm, Sigma sigma, Theta theta) {
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
        erlCode.append("-type state_data() :: #state_data{}.\n\n");
        genStartLinkFunction(erlCode, lowercaseRole);
        genStateFunctions(erlCode, fsm, lowercaseRole, sigma, theta);
    }

    private static void genStartLinkFunction(StringBuilder erlCode, String role) {
        erlCode.append(String.format("%% @doc Starts the %s process.\n", role));
        erlCode.append("-spec start_link() -> {ok, pid()} | {error, any()}.\n");
        erlCode.append("start_link() ->\n");
        erlCode.append(String.format("  gen_%s:start_link(?MODULE, []).\n\n", role));
    }

    /**
     * Generates state functions for the callback module.
     */
    private static void genStateFunctions(StringBuilder erlCode, StateM fsm, String role, Sigma sigma, Theta theta) {
        // Generate protocol-specific logic for each state
        Set<State> states = fsm.getStates();
        for (State state : states) {
            switch (state.getKind()){
                case MIXED_INTERNAL:
                    //get the RHS transition to name the choice
                    generateMakeChoiceFunction(erlCode, state.getTransitions().stream().reduce((first, second) ->
                            second).get().getEvent().getName());
                case MIXED_EXTERNAL:

                case REC:
                case EXTERNAL:

                case INTERNAL:
                    genInternalStateFunction(erlCode, state, role);
                    break;
                case INIT:
                    genInitStateFunction(erlCode, state, role, sigma, theta);
                    break;
                case TERMINAL:
                default:
                    erlCode.append("\n");
                    break;
            }
        }



    }

private static void genInternalStateFunction(StringBuilder erlCode, State state, String role) {
        erlCode.append(String.format("%% @doc %s for %s.\n", state.getName(), role));
        erlCode.append(String.format("-spec %s(atom(), ", state.getName()));
        int index = erlCode.length();

        StringBuilder resultTypes = new StringBuilder();
        StringBuilder messageTypes = new StringBuilder();

        for (Transition t : state.getTransitions()) {
            handleTransition(erlCode, t, state, role, resultTypes, messageTypes);
        }

    //    addDefaultCase(erlCode, state);
        appendSpecSignature(erlCode, index, messageTypes, resultTypes);
        erlCode.replace(erlCode.length() - 2, erlCode.length(), ".\n\n");
}

    private static void handleTransition(StringBuilder erlCode, Transition t, State state, String role,
                                         StringBuilder resultTypes, StringBuilder messageTypes) {
        String event = t.getEvent().getName().toLowerCase();

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
                handleReceiveTransition(erlCode, t, state, resultTypes, messageTypes);
                break;
        }
    }

    private static void handleMixedSendTransition(StringBuilder erlCode,Transition t, State state,
                                                  String role, StringBuilder resultTypes) {
        String event = t.getEvent().getName().toLowerCase();
        String dst = t.getEvent().getRole().toString();

        erlCode.append(String.format("%s(internal, %s_choice, #state_data{%s_pid = %sPid} = Data) ->\n",
                state.getName(), event, lowercaseFirstLetter(dst), dst));

        erlCode.append(indent(1)).append("io:format(\"Making a choice~n\"),\n");
        erlCode.append(indent(1)).append(String.format("Choice = make_%s_choice(Data),\n", event));
        erlCode.append(indent(1)).append("case Choice of\n");
        erlCode.append(indent(2)).append("1 ->\n");
        erlCode.append(indent(3)).append("{keep_state, Data};\n");
        erlCode.append(indent(2)).append("2 ->\n");
        erlCode.append(indent(3)).append("io:format(\"Sending ~p~n\", [").append(event).append("]),\n");
        if(t.getEvent().getMc() != 0 ){
            erlCode.append(indent(3)).append(String.format("gen_%s:send_%s(%sPid, Data),\n", role, event, dst));

        } else {
            erlCode.append(indent(3)).append(String.format("gen_%s:send_%s(%sPid),\n", role, event, dst));
        }
        State nextState = t.getNextState();
        if(nextState.getKind().equals(StateKind.TERMINAL)){
            erlCode.append(indent(3)).append("{stop, normal, Data}\n");
        } else {
            erlCode.append(indent(3)).
                    append(String.format("{next_state, %s, Data",
                            nextState.getName()));
            if(nextState.getKind().equals(StateKind.INTERNAL)){
                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
                erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName().toLowerCase()));
            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
                // get the last transition in the next state and add it as the next event
                //TODO better way to get rhs of mixed choice
                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName().toLowerCase()));
            }
            erlCode.append("}\n");

        }
        erlCode.append(indent(2)).append("end;\n");

        resultTypes.append("{keep_state, state_data()} | \n").append(indent(1)).append("{next_state, ").
                append(state.getName()).append(", state_data()} | \n").append(indent(1));
    }

    private static void generateMakeChoiceFunction(StringBuilder erlCode, String event) {
        erlCode.append(String.format("make_%s_choice(_Data) ->\n", event));

        erlCode.append(indent(1)).append("io:format(\"Making a choice~n\"),\n");
        erlCode.append(indent(1)).append("rand:uniform(2).\n\n");
    }

    private static void handleSendTransition(StringBuilder erlCode, Transition t, State state, String role,
                                             StringBuilder resultTypes) {
        String event = t.getEvent().getName().toLowerCase();
        String dst = t.getEvent().getRole().toString();
        State nextState = t.getNextState();
        erlCode.append(String.format("%s(internal, {%s}, #state_data{%s_pid = %sPid} = Data) ->\n", state.getName(),
                event, lowercaseFirstLetter(dst), dst));
        erlCode.append(indent(1)).append("io:format(\"Sending ~p~n\", [").append(event).append("]),\n");
        if(t.getEvent().getMc() != 0 ){
            erlCode.append(indent(1)).append(String.format("gen_%s:send_%s(%sPid, Data),\n", role, event, dst));

        } else {
            erlCode.append(indent(1)).append(String.format("gen_%s:send_%s(%sPid),\n", role, event, dst));
        }

        if(nextState.getKind().equals(StateKind.TERMINAL)){
            resultTypes.append("{stop, normal, state_data()} | \n").append(indent(1));
            erlCode.append(indent(1)).append("{stop, normal, Data};\n");
        } else {
            resultTypes.append(String.format("{next_state, %s, state_data()", nextState.getName()));
            erlCode.append(indent(1)).
                    append(String.format("{next_state, %s, Data",
                            nextState.getName()));
            if(nextState.getKind().equals(StateKind.INTERNAL)){
                resultTypes.append(", [term()]");
                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
                erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName().toLowerCase()));
            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
                resultTypes.append(", [term()]");
                // get the last transition in the next state and add it as the next event
                //TODO better way to get rhs of mixed choice
                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName().toLowerCase()));
            }
            resultTypes.append("} | \n").append(indent(1));
            erlCode.append("};\n");

        }
    }

    private static void handleReceiveTransition(StringBuilder erlCode, Transition t, State state,
                                                StringBuilder resultTypes, StringBuilder messageTypes) {
        String event = t.getEvent().getName().toLowerCase();
        String dst = t.getEvent().getRole().toString();
        State nextState = t.getNextState();
        erlCode.append(String.format("%s(cast, {%sPid, {%s}}, #state_data{%s_pid = %sPid} = Data) ->\n",
                state.getName(), dst, event, lowercaseFirstLetter(dst), dst));
        messageTypes.append(String.format("{pid(), {%s}} | " , event));

        if (nextState.getKind().equals(StateKind.TERMINAL)) {
            erlCode.append(indent(1)).append("{stop, normal, Data};\n");
            resultTypes.append("{stop, normal, state_data()} | \n").append(indent(1));
        } else {
            resultTypes.append(String.format("{next_state, %s, state_data()", nextState.getName()));
            erlCode.append(indent(1)).
                    append(String.format("{next_state, %s, Data",
                            nextState.getName()));
            if(nextState.getKind().equals(StateKind.INTERNAL)){
                resultTypes.append(", [term()]");
                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
                erlCode.append(String.format(", [{next_event, internal, {%s}}]",
                        nextTransition.getEvent().getName().toLowerCase()));
            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
                resultTypes.append(", [term()]");
                // get the last transition in the next state and add it as the next event
                //TODO better way to get rhs of mixed choice
                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName().toLowerCase()));
            }
            resultTypes.append("} | \n").append(indent(1));
            erlCode.append("};\n");

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

//    private static void genInitStateFunction(StringBuilder erlCode, State state, String role) {
//        State nextState = state.getTransitions().stream().findFirst().get().getNextState();
//        //TODO: if nextState after init is internal a new state needs to be added.
//        if(nextState.getKind().equals(StateKind.TERMINAL)){
//            erlCode.append(String.format("%% @doc Initializes the %s state machine.\n", role));
//            erlCode.append("-spec init(list()) -> {ok, _}.\n");
//            erlCode.append("init([]) ->\n");
//            erlCode.append(indent(1)).append(String.format("{ok, %s}.\n\n", nextState.getName()));
//        } else {
//
//            if(nextState.getKind().equals(StateKind.INTERNAL)){
//                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
//                erlCode.append(String.format("-spec init(list()) -> {ok, %s, state_data(), [term()]}.\n", nextState.getName()));
//                erlCode.append("init([]) ->\n");
//                erlCode.append(indent(1)).append(String.format("io:format(\"%s initialized~n\"),\n", role));
//                erlCode.append(indent(1)).append(String.format("{ok, %s, #state_data{}", nextState.getName()));
//                erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName().toLowerCase()));
//            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL) ||
//                        nextState.getKind().equals(StateKind.MIXED_EXTERNAL)){
//                erlCode.append(String.format("-spec init(list()) -> {ok, %s, state_data(), [term()]}.\n", nextState.getName()));
//                erlCode.append("init([]) ->\n");
//                erlCode.append(indent(1)).append(String.format("{ok, %s, #state_data{}", nextState.getName()));
//                // get the last transition in the next state and add it as the next event
//                //TODO better way to get rhs of mixed choice
//                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
//                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName().toLowerCase()));
//            } else {
//                erlCode.append(String.format("-spec init(list()) -> {ok, %s, state_data()}.\n", nextState.getName()));
//                erlCode.append("init([]) ->\n");
//                erlCode.append(indent(1)).append(String.format("{ok, %s, #state_data{}", nextState.getName()));
//            }
//            erlCode.append("}.\n\n");
//        }
//    }


    private static void genInitStateFunction(StringBuilder erlCode, State state, String role, Sigma sigma, Theta theta) {
        State nextState = state.getTransitions().stream().findFirst().get().getNextState();
        String nextStateName = nextState.getName();
        String initSpec = String.format("-spec init(list()) -> {ok, %s", nextStateName);

        erlCode.append(String.format("%% @doc Initializes the %s state machine.\n", role));

        if (nextState.getKind().equals(StateKind.TERMINAL)) {
            erlCode.append(initSpec).append(", _}.\n");
            erlCode.append("init([]) ->\n");
            erlCode.append(indent(1)).append(String.format("{ok, %s}.\n\n", nextStateName));
        } else {
            erlCode.append(initSpec).append(", state_data()");
            if (nextState.getKind().equals(StateKind.INTERNAL) || nextState.getKind().equals(StateKind.MIXED_INTERNAL) ||
                    nextState.getKind().equals(StateKind.MIXED_EXTERNAL)) {
                erlCode.append(", [term()]}.\n");
            } else {
                erlCode.append("}.\n");
            }

            erlCode.append("init([]) ->\n");

            for (Role r : sigma.map.keySet()) {
                erlCode.append(indent(1)).append(String.format("%sPid = ", r));
                erlCode.append("case whereis(").append(r.toString().toLowerCase()).append(") of\n");
                erlCode.append(indent(2)).append("undefined ->\n");
                erlCode.append(indent(3)).append("io:format(\"").append(r.toString().toLowerCase()).append(" is not available yet. Will retry...~n\"),\n");
                erlCode.append(indent(3)).append("timer:sleep(1000),\n");
                erlCode.append(indent(3)).append("whereis(").append(r.toString().toLowerCase()).append(");\n");
                erlCode.append(indent(2)).append("Pid -> Pid\n");
                // update state state_data with the pid
                erlCode.append(indent(2)).append("end,\n\n");
            }


            erlCode.append(indent(1)).append("Data = #state_data{");
            for (Role r : sigma.map.keySet()) {
                erlCode.append(r.toString().toLowerCase()).append("_pid = ").append(r).append("Pid, ");
            }
//            erlCode.append("mc_counter_1 = 0},\n");
            for (int i = 1; i <= theta.map.size(); i++) {
                erlCode.append("mc_counter_").append(i).append(" = 0, ");
            }
            erlCode.replace(erlCode.length() - 2, erlCode.length(), "},\n");

            erlCode.append(indent(1)).append(String.format("io:format(\"%s initialized~n\"),\n", role));
            erlCode.append(indent(1)).append(String.format("{ok, %s, Data", nextStateName));

            if (nextState.getKind().equals(StateKind.INTERNAL) || nextState.getKind().equals(StateKind.MIXED_EXTERNAL)) {
                Transition nextTransition = nextState.getTransitions().stream().findFirst().get();
                erlCode.append(String.format(", [{next_event, internal, {%s}}]", nextTransition.getEvent().getName().toLowerCase()));
            } else if (nextState.getKind().equals(StateKind.MIXED_INTERNAL)) {
                Transition rhs = nextState.getTransitions().stream().reduce((first, second) -> second).get();
                erlCode.append(String.format(", [{next_event, internal, %s_choice}]", rhs.getEvent().getName().toLowerCase()));
            }

            erlCode.append("}.\n\n");
        }
    }


}
