package org.scribble.gt.codegen;

import java.util.ArrayList;
import java.util.Set;

import static org.scribble.gt.codegen.CodeGenUtils.indent;
import static org.scribble.gt.codegen.ErlangCodeGen.lowercaseFirstLetter;

public class GenericBehaviour {
    /**
     * Generates Erlang generic behaviour code Erlang code for a local type.
     */
    public static void genGenericBehavior(String role, StringBuilder erlCode, StateM fsm) {
        String lowercaseRole = lowercaseFirstLetter(role);
        String genericModuleName = "gen_" + lowercaseRole;
        erlCode.append("-module(").append(genericModuleName).append(").\n");
        erlCode.append("-behaviour(gen_statem).\n\n");

        erlCode.append("-export([start_link/2, callback_mode/0, init/2, terminate/3, flush_mailbox/0, flush_messages/0 ")
                .append("]).\n\n");
        erlCode.append("-export([");
        for (String fun : stateTransitionFuns(fsm)){
            erlCode.append("send_").append(fun).append("/1, ");
        }
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
        genStateTransitionFunctions(erlCode, fsm);
        genStateFunctions(erlCode, fsm, cBackIndex);
        genTerminateFunction(erlCode);
        genUtilityFunctions(erlCode);
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
            erlCode.append("init(CallbackModule, Args) ->\n");
            erlCode.append(indent(1)).append(
                    String.format(
                            "io:format(\"%s: Initializing with callback module ~p~n\", [CallbackModule]),\n", moduleName));
            erlCode.append(indent(1)).append("put(callback_module, CallbackModule),\n");
            erlCode.append(indent(1)).append(String.format("{ok, %s, #state_data{}}.\n\n", nextState));
        }
    }

    private static ArrayList<String> stateTransitionFuns(StateM fsm) {
        ArrayList<String> funs = new ArrayList<>();
        for (Transition t : fsm.getTransitions()){
            if(t.getEvent().getKind().equals(EventKind.SEND) || t.getEvent().getKind().equals(EventKind.MIXED_SEND)){
                funs.add(t.getEvent().getName());
            }
        }
        return funs;
    }

    private static void genStateTransitionFunctions(StringBuilder erlCode, StateM fsm) {
        for (Transition t : fsm.getTransitions()){
            String destRole = t.getEvent().getRole();
//            if(!destRole.equals(fsm.getName())) // probably not needed
                if(t.getEvent().getKind().equals(EventKind.SEND) || t.getEvent().getKind().equals(EventKind.MIXED_SEND)){
                    String event = t.getEvent().getName();
                    erlCode.append(String.format("-spec send_%s(pid()) -> ok().\n", event));
                    erlCode.append(String.format("send_%s(%sPid) -> \n", event, destRole));
                    erlCode.append(indent(1)).append(String.format("gen_statem:cast(%sPid, {self(), {%s}}).\n \n",
                            destRole, event));
                }
        }
    }

    //todo - add mixed choice counter to relevant messages
    private static void genStateFunction(StringBuilder erlCode, State state, int cIndex) {
        // for each transition generate a spec with the corresponding message type and return type;
        // if the next state is terminal, return stop;
        // generate a function that calls the callback module with the event type, message and data
        erlCode.append(String.format("-spec %s(atom(), ", state.getName()));
        int index = erlCode.length();
        StringBuilder resultTypes = new StringBuilder();
        StringBuilder messageTypes = new StringBuilder();
        StringBuilder functionDef = new StringBuilder();
        for(Transition t : state.getTransitions()){
            String event = t.getEvent().getName();
            State nextState = t.getNextState();

            if (t.getEvent().getKind().equals(EventKind.MIXED_SEND)){
                messageTypes.append(String.format(" %s_choice", event));
            } else {
                messageTypes.append(String.format("{pid(), {%s}}", event));
            }
            messageTypes.append(" | ");
            if(nextState.getKind().equals(StateKind.TERMINAL)){
                resultTypes.append("{stop, normal, state_data()}");
            } else {
                resultTypes.append(String.format("{next_state, %s, state_data()}", nextState.getName()));
            }
            resultTypes.append(" | ");
            erlCode.append(String.format("%s(EventType, {%sPid, {%s}}, Data) ->\n",
                    state.getName(), t.getEvent().getRole(), event));
            erlCode.append(indent(1)).append("CallbackModule = get(callback_module),\n");
            erlCode.append(indent(1)).
                    append(String.format("CallbackModule:%s(EventType, {%sPid, {%s}}, Data);\n",
                            state.getName(), t.getEvent().getRole(), event));
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


    private static void genStateFunctions(StringBuilder erlCode, StateM fsm, int cIndex) {
        Set<State> states = fsm.getStates();
        for (State state : states) {
            if(!state.getKind().equals(StateKind.TERMINAL) && !state.getKind().equals(StateKind.INIT)){
                genStateFunction(erlCode, state, cIndex);
            }
        }
    }

    private static void genTerminateFunction(StringBuilder erlCode) {
        erlCode.append("terminate(_Reason, _State, _Data) ->\n");
        erlCode.append(indent(1)).append("io:format(\"Terminating ~p~n\", [self()]),\n");
        erlCode.append(indent(1)).append("flush_mailbox(),\n");
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
