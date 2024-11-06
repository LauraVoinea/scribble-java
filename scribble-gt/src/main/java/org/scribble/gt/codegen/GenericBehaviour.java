package org.scribble.gt.codegen;

import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.scribble.gt.codegen.CodeGenUtils.indent;
import static org.scribble.gt.codegen.ErlangCodeGen.lowercaseFirstLetter;

public class GenericBehaviour {
    /**
     * Generates Erlang generic behaviour code Erlang code for a local type.
     */
    public static void genGenericBehavior(String role, StringBuilder erlCode, StateM fsm, boolean explicitGC) {
        String lowercaseRole = lowercaseFirstLetter(role);
        String genericModuleName = "gen_" + lowercaseRole;
        erlCode.append("-module(").append(genericModuleName).append(").\n");
        erlCode.append("-behaviour(gen_statem).\n\n");

        erlCode.append("-export([start_link/2, callback_mode/0, init/1, terminate/3")
                .append("]).\n\n");
        erlCode.append("-export([");
        int exportIndex = erlCode.length();
        erlCode.append("]).\n");
        erlCode.append("-export([");
        for (State state : fsm.getStates() ){
            if(!state.getKind().equals(StateKind.TERMINAL) && !state.getKind().equals(StateKind.INIT)){
                erlCode.append(state.getName()).append("/3, ");
            }
        }
        erlCode.replace(erlCode.length() - 2, erlCode.length(), "]).\n\n");


        erlCode.append(String.format("-include(\"%s.hrl\").\n\n", lowercaseRole));

        erlCode.append("-type state_data() :: #state_data{}.\n\n");
        int cBackIndex = erlCode.length();
        genStartLinkFunction(erlCode, genericModuleName);
        genCallbackModeFunction(erlCode);
        genInitFunction(erlCode, fsm.getInitState(), genericModuleName);
        int offset = genStateTransitionFunctions(erlCode, fsm, explicitGC, exportIndex);
        genStateFunctions(erlCode, fsm, cBackIndex + offset, explicitGC);
        genTerminateFunction(erlCode);
//        genUtilityFunctions(erlCode);
    }

    private static void genStartLinkFunction(StringBuilder erlCode, String moduleName) {
        erlCode.append("-spec start_link(module(), list()) -> {ok, pid()} | {error, any()}.\n");
        erlCode.append("start_link(CallbackModule, Args) ->\n");

        erlCode.append(indent(1)).append("case code:ensure_loaded(CallbackModule) of\n");
        erlCode.append(indent(2)).append("{module, CallbackModule} ->\n");
        erlCode.append(indent(3)).append(String.format("gen_statem:start_link({local, CallbackModule}, %s, {CallbackModule, Args}, []);\n", moduleName));
        erlCode.append(indent(2)).append("{error, Reason} ->\n");
        erlCode.append(indent(3)).append("{error, Reason}\n");
        erlCode.append(indent(1)).append("end.\n\n");
    }

    private static void genCallbackModeFunction(StringBuilder erlCode) {
        erlCode.append("callback_mode() ->\n");
        erlCode.append(indent(1)).append("state_functions.\n\n");
    }

    private static void genInitFunction(StringBuilder erlCode, State state, String moduleName) {
        if(state != null) {
            String nextState = state.getTransitions().stream().findFirst().get().getNextState().getName();
            erlCode.append(String.format("-spec init({module(), list()}) -> {ok, %s, state_data()}.\n", nextState));
            erlCode.append("init({CallbackModule, _Args}) ->\n");
            erlCode.append(indent(1)).append(
                    String.format(
                            "io:format(\"%s: Initializing with callback module ~p~n\", [CallbackModule]),\n", moduleName));
            erlCode.append(indent(1)).append("put(callback_module, CallbackModule),\n");
            erlCode.append(indent(1)).append("CallbackModule:init([]). \n\n");
        }
    }


    // todo function signature for export
    private static int genStateTransitionFunctions(StringBuilder erlCode, StateM fsm, boolean explicitGC, int exportIndex) {
        StringBuilder funs = new StringBuilder();
        for (Transition t : fsm.getTransitions()) {
            String destRole = t.getEvent().getRole();
            if (t.getEvent().getKind() == EventKind.SEND || t.getEvent().getKind() == EventKind.MIXED_SEND) {
                funs.append(String.format("send_%s/", t.getEvent().getName().toLowerCase()));
                String event = t.getEvent().getName().toLowerCase();
                boolean hasGcEvents = t.getEvent().getMc() != 0;

                if (hasGcEvents && explicitGC) {
                    funs.append("2, ");
                    erlCode.append(String.format("-spec send_%s(pid(), state_data()) -> ok.\n", event));
                    erlCode.append(String.format("send_%s(%sPid, Data) ->\n", event, destRole));
                    erlCode.append(indent(1)).
                            append(String.format("Counter = Data#state_data.mc_counter_%s,\n", t.getEvent().getMc()));
                    erlCode.append(indent(1)).append(
                            String.format("gen_statem:cast(%sPid, {self(), {%s}, Counter}).\n\n", destRole, event));
                } else {
                    funs.append("1, ");
                    erlCode.append(String.format("-spec send_%s(pid()) -> ok.\n", event));
                    erlCode.append(String.format("send_%s(%sPid) ->\n", event, destRole));
                    erlCode.append(indent(1)).append(
                            String.format("gen_statem:cast(%sPid, {self(), {%s}}).\n\n", destRole, event));
                }
            }
        }
        erlCode.insert(exportIndex, funs.substring(0, funs.length() - 2));
        return funs.length() > 0 ? funs.length() - 2 : 0;
    }


//    private static void genStateFunction(StringBuilder erlCode, State state, int cIndex, boolean explicitGC) {
//        // for each transition generate a spec with the corresponding message type and return type;
//        // if the next state is terminal, return stop;
//        // generate a function that calls the callback module with the event type, message and data
//        erlCode.append(String.format("-spec %s(atom(), ", state.getName()));
//        int index = erlCode.length();
//        StringBuilder resultTypes = new StringBuilder();
//        StringBuilder messageTypes = new StringBuilder();
//        StringBuilder functionDef = new StringBuilder();
//        boolean hasGcEvents = !state.getGcEvents().isEmpty();
//
//        for(Transition t : state.getTransitions()){
//            String event = t.getEvent().getName().toLowerCase();
//            State nextState = t.getNextState();
//
//            if (t.getEvent().getKind().equals(EventKind.MIXED_SEND)){
//                messageTypes.append(String.format(" %s_choice", event));
//                erlCode.append(String.format("%s(EventType, %s_choice, Data) ->\n",
//                        state.getName(), event));
//                erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
//                erlCode.append(indent(1)).
//                        append(String.format("CallbackModule:%s(EventType, %s_choice, Data);\n",
//                                state.getName(), event));
//            } else if (t.getEvent().getKind().equals(EventKind.SEND)){
//                messageTypes.append(String.format("{%s}", event));
//                erlCode.append(String.format("%s(EventType, {%s}, Data) ->\n",
//                        state.getName(), event));
//                erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
//                erlCode.append(indent(1)).
//                        append(String.format("CallbackModule:%s(EventType, {%s}, Data);\n",
//                                state.getName(), event));
//
//            } else {
//                if (hasGcEvents && explicitGC) {
//                    // Handle valid messages
//                    messageTypes.append(String.format("{pid(), {%s}}", event));
//                    erlCode.append(String.format("%s(EventType, {%sPid, {%s}, Counter}, " +
//                                    "Data = #state_data{mc_counter_%s = MC}) when Counter < MC ->\n",
//                            state.getName(), t.getEvent().getRole(), event, t.getEvent().getMc()));
//                    erlCode.append(indent(1)).append(
//                            String.format("NewData = Data#state_data{mc_counter_%s = Counter + 1},\n",
//                                    t.getEvent().getMc()));
//                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
//                    erlCode.append(indent(1)).
//                            append(String.format("CallbackModule:%s(EventType, {%sPid, {%s}}, NewData);\n",
//                                    state.getName(), t.getEvent().getRole(), event));
//
//                    // Discard outdated messages
//                    for (Event e: state.getGcEvents()) {
//                        erlCode.append(
//                                String.format("%s(cast, {_%sPid, {%s}, Counter}, Data = #state_data{mc_counter_%s = MC}) when Counter < MC ->\n",
//                                state.getName(), e.getRole(), e.getName(), e.getMc()));
//                        erlCode.append(indent(1)).append("% Discard outdated message\n");
//                        erlCode.append(indent(1)).append("{keep_state, Data};\n");
//                    }
//                } else {
//                    // Standard message handling
//                    messageTypes.append(String.format("{pid(), {%s}}", event));
//                    erlCode.append(String.format("%s(cast, {%sPid, {%s}}, Data) ->\n",
//                            state.getName(), t.getEvent().getRole(), event));
//                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
//                    erlCode.append(indent(1)).
//                            append(String.format("CallbackModule:%s(cast, {%sPid, {%s}}, Data);\n",
//                                    state.getName(), t.getEvent().getRole(), event));
//                    if(hasGcEvents && !explicitGC){
//                        for (Event e: state.getGcEvents()) {
//                            if(!e.getName().equals(event)) {
//                                erlCode.append(
//                                        String.format("%s(cast, {_%sPid, {%s}}, Data) ->\n",
//                                                state.getName(), e.getRole(), e.getName()));
//                                erlCode.append(indent(1)).append("% Discard outdated message\n");
//                                erlCode.append(indent(1)).append("{keep_state, Data};\n");
//                            }
//                        }
//                }
//                }
//            }
//            messageTypes.append(" | ");
//            if(nextState.getKind().equals(StateKind.TERMINAL)){
//                resultTypes.append("{stop, normal, state_data()}");
//            } else {
//                resultTypes.append(String.format("{next_state, %s, state_data()}", nextState.getName()));
//            }
//            resultTypes.append(" | ");
//        }
//
//        erlCode.replace(erlCode.length() - 2, erlCode.length(), ".\n\n");
//        erlCode.insert(index, messageTypes.substring(0, messageTypes.length() - 3)
//                + ", state_data()) -> " + resultTypes.substring(0, resultTypes.length() - 3) + ".\n");
//
//        functionDef.append(String.format("%s(atom(),", state.getName())).
//                append(messageTypes.substring(0, messageTypes.length() - 3)).
//                append(", state_data()) -> ");
//        functionDef.append(resultTypes.substring(0, resultTypes.length() - 3)).append(".\n");
//        // generate the callback function declaration
//        erlCode.insert(cIndex, "-callback " + functionDef + "\n");
//
//    }


        private static void genStateFunction(StringBuilder erlCode, State state, int cIndex, boolean explicitGC) {
        // for each transition generate a spec with the corresponding message type and return type;
        // if the next state is terminal, return stop;
        // generate a function that calls the callback module with the event type, message and data
        erlCode.append(String.format("-spec %s(atom(), ", state.getName()));
        int index = erlCode.length();
        StringBuilder resultTypes = new StringBuilder();
        StringBuilder messageTypes = new StringBuilder();
        StringBuilder functionDef = new StringBuilder();
        boolean hasGcEvents = !state.getGcEvents().isEmpty();

        for(Transition t : state.getTransitions()){
            String event = t.getEvent().getName().toLowerCase();
            State nextState = t.getNextState();

            if (t.getEvent().getKind().equals(EventKind.MIXED_SEND)){
                messageTypes.append(String.format(" %s_choice", event));
                if(explicitGC){
                    erlCode.append(String.format("%s(EventType, %s_choice, Data = #state_data{mc_counter_%s = MC}) ->\n",
                            state.getName(), event, t.getEvent().getMc()));
                    erlCode.append(indent(1)).append(
                            String.format("NewData = Data#state_data{mc_counter_%s = MC + 1},\n",
                                    t.getEvent().getMc()));
                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                    erlCode.append(indent(1)).
                            append(String.format("CallbackModule:%s(EventType, %s_choice, NewData);\n",
                                    state.getName(), event));
                } else {
                    erlCode.append(String.format("%s(EventType, %s_choice, Data) ->\n",
                            state.getName(), event));
                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                    erlCode.append(indent(1)).
                            append(String.format("CallbackModule:%s(EventType, %s_choice, Data);\n",
                                    state.getName(), event));
                }

            } else if (t.getEvent().getKind().equals(EventKind.SEND)){
                messageTypes.append(String.format("{%s}", event));
                if(explicitGC && state.getKind().equals(StateKind.MIXED_EXTERNAL)){
                    erlCode.append(String.format("%s(EventType, {%s}, Data = #state_data{mc_counter_%s = MC}) ->\n",
                            state.getName(), event, t.getEvent().getMc()));
                    erlCode.append(indent(1)).append(
                            String.format("NewData = Data#state_data{mc_counter_%s = MC + 1},\n",
                                    t.getEvent().getMc()));
                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                    erlCode.append(indent(1)).
                            append(String.format("CallbackModule:%s(EventType, {%s}, NewData);\n",
                                    state.getName(), event));
                }else {
                    erlCode.append(String.format("%s(EventType, {%s}, Data) ->\n",
                            state.getName(), event));
                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                    erlCode.append(indent(1)).
                            append(String.format("CallbackModule:%s(EventType, {%s}, Data);\n",
                                    state.getName(), event));
                }
            } else {
                //event is part of a mixed choice
                if (t.getEvent().getMc() > 0 && explicitGC) {
                    // Handle valid messages
                    messageTypes.append(String.format("{pid(), {%s}, integer()} | ", event));
                    messageTypes.append(String.format("{pid(), {%s}}", event));

                    erlCode.append(String.format("%s(EventType, {%sPid, {%s}, Counter}, " +
                                    "Data = #state_data{mc_counter_%s = MC}) when Counter >= MC ->\n",
                            state.getName(), t.getEvent().getRole(), event, t.getEvent().getMc()));
                    if (state.getKind().equals(StateKind.MIXED_EXTERNAL)) {
                        erlCode.append(indent(1)).append(
                                String.format("NewData = Data#state_data{mc_counter_%s = MC + 1},\n",
                                        t.getEvent().getMc()));

                        erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                        erlCode.append(indent(1)).
                                append(String.format("CallbackModule:%s(EventType, {%sPid, {%s}}, NewData);\n",
                                        state.getName(), t.getEvent().getRole(), event));
                    } else {
                        erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                        erlCode.append(indent(1)).
                                append(String.format("CallbackModule:%s(EventType, {%sPid, {%s}}, Data);\n",
                                        state.getName(), t.getEvent().getRole(), event));
                    }

                 Optional<Event> gcEvent = t.getCurrentState().getGcEvents().stream()
                            .filter(e -> e.getName().equals(event))
                            .findFirst();
                    // Discard outdated messages
                    if (gcEvent.isPresent()) {
                        Event e = gcEvent.get();
                        messageTypes.append(" | ");
                        messageTypes.append(String.format("{_%sPid, {%s}, integer()}", e.getRole(), e.getName().toLowerCase()));
                        resultTypes.append("{keep_state, state_data()} | ");

                        erlCode.append(
                                String.format("%s(_EventType, {_%sPid, {%s}, Counter}, Data = #state_data{mc_counter_%s = MC}) when Counter < MC ->\n",
                                state.getName(), e.getRole(), e.getName(), e.getMc()));
                        erlCode.append(indent(1)).append("% Discard outdated message\n");
                        erlCode.append(indent(1)).append("{keep_state, Data};\n");
                    }
                } else {
                    // Standard message handling
                    messageTypes.append(String.format("{pid(), {%s}}", event));

                    erlCode.append(String.format("%s(cast, {%sPid, {%s}}, Data) ->\n",
                            state.getName(), t.getEvent().getRole(), event));
                    erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
                    erlCode.append(indent(1)).
                            append(String.format("CallbackModule:%s(cast, {%sPid, {%s}}, Data);\n",
                                    state.getName(), t.getEvent().getRole(), event));
                }
            }

            messageTypes.append(" | ");
            if(nextState.getKind().equals(StateKind.TERMINAL)){
                resultTypes.append("{stop, normal, state_data()}");
            } else if (nextState.getKind().equals(StateKind.INTERNAL)
                    || nextState.getKind().equals(StateKind.MIXED_INTERNAL)){
                resultTypes.append(String.format("{next_state, %s, state_data()} | ", nextState.getName()));
                resultTypes.append(String.format("{next_state, %s, state_data(), [term()]}", nextState.getName()));
            } else {
                resultTypes.append(String.format("{next_state, %s, state_data()}", nextState.getName()));
            }
            resultTypes.append(" | ");
        }
            //gcEvents = state.getGcEvents with all the events from the transitions removed
            Set<Event> gcEvents = state.getGcEvents().stream()
                    .filter(e -> !state.getTransitions().stream()
                            .map(Transition::getEvent)
                            .map(Event::getName)
                            .anyMatch(n -> n.equals(e.getName())))
                    .collect(Collectors.toSet());

            // Discard outdated messages
            if (hasGcEvents) {
                if (explicitGC) {
                    for (Event e : gcEvents) {
                        messageTypes.append(String.format("{_%sPid, {%s}, integer()}", e.getRole(), e.getName().toLowerCase()));
                        messageTypes.append(" | ");
                        erlCode.append(
                                String.format("%s(_EventType, {_%sPid, {%s}, _Counter}, Data) ->\n",
                                        state.getName(), e.getRole(), e.getName().toLowerCase()));
                        erlCode.append(indent(1)).append("% Discard outdated message\n");
                        erlCode.append(indent(1)).append("{keep_state, Data};\n");
                    }
                } else{
                    for (Event e: gcEvents) {
                        messageTypes.append(String.format(" {_%sPid, {%s}}", e.getRole(), e.getName()));
                        messageTypes.append(" | ");
                            erlCode.append(
                                    String.format("%s(_EventType, {_%sPid, {%s}}, Data) ->\n",
                                            state.getName(), e.getRole(), e.getName()));
                            erlCode.append(indent(1)).append("% Discard outdated message\n");
                            erlCode.append(indent(1)).append("{keep_state, Data};\n");
                        }
                }
                resultTypes.append("{keep_state, state_data()} | ");
            }

        erlCode.replace(erlCode.length() - 2, erlCode.length(), ".\n\n");
        erlCode.insert(index, messageTypes.substring(0, messageTypes.length() - 3)
                + ", state_data()) -> " + resultTypes.substring(0, resultTypes.length() - 3) + ".\n");

        functionDef.append(String.format("%s(atom(),", state.getName())).
                append(messageTypes.substring(0, messageTypes.length() - 3)).
                append(", state_data()) -> ");
        functionDef.append(resultTypes.substring(0, resultTypes.length() - 3)).append(".\n");
        // generate the callback function declaration
        erlCode.insert(cIndex, "-callback " + functionDef + "\n");

    }

    private static void genStateFunctions(StringBuilder erlCode, StateM fsm, int cIndex, boolean explicitGC) {
        Set<State> states = fsm.getStates();
        for (State state : states) {
            if(!state.getKind().equals(StateKind.TERMINAL) && !state.getKind().equals(StateKind.INIT)){
                genStateFunction(erlCode, state, cIndex, explicitGC);
            }
        }
    }

    private static void genTerminateFunction(StringBuilder erlCode) {
        erlCode.append("terminate(_Reason, _State, _Data) ->\n");
        erlCode.append(indent(1)).append("io:format(\"Terminating ~p~n\", [self()]),\n");
//        erlCode.append(indent(1)).append("flush_mailbox(),\n");
        erlCode.append(indent(1)).append("ok.\n\n");
    }

    private static void genUtilityFunctions(StringBuilder erlCode) {
        erlCode.append("flush_mailbox() ->\n");
        erlCode.append(indent(1)).append("receive\n");
        erlCode.append(indent(2)).append("_ -> flush_mailbox()\n");
        erlCode.append(indent(1)).append("after 0 ->\n");
        erlCode.append(indent(2)).append("ok\n");
        erlCode.append(indent(1)).append("end.\n\n");

        erlCode.append("flush_messages() ->\n");
        erlCode.append(indent(1)).append("receive\n");
        erlCode.append(indent(2)).append("{ping, _} -> flush_messages();\n");
        erlCode.append(indent(2)).append("_OtherMessage -> \n");
        erlCode.append(indent(3)).append("self() ! _OtherMessage,\n");
        erlCode.append(indent(3)).append("flush_messages()\n");
        erlCode.append(indent(1)).append("after 0 ->\n");
        erlCode.append(indent(2)).append("ok\n");
        erlCode.append(indent(1)).append("end.\n\n");
    }
}
