-module(gen_c).
-behaviour(gen_statem).

-export([start_link/2, callback_mode/0, init/1, terminate/3]).

-export([state1/3, state2/3]).

-include("c.hrl").

-type state_data() :: #state_data{}.

-callback state2(atom(),{pid(), {6}}, state_data()) -> {stop, normal, state_data()}.

-callback state1(atom(),{pid(), {2}} | {pid(), {exception}}, state_data()) -> {next_state, state2, state_data()} | {stop, normal, state_data()}.

-spec start_link(module(), list()) -> {ok, pid()} | {error, any()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

callback_mode() ->
    state_functions.

-spec init({module(), list()}) -> {ok, state1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("gen_c: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec state1(atom(), {pid(), {2}} | {pid(), {exception}}, state_data()) -> {next_state, state2, state_data()} | {stop, normal, state_data()}.
state1(EventType, {BPid, {2}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state1(EventType, {BPid, {2}}, Data);
state1(EventType, {BPid, {exception}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state1(EventType, {BPid, {exception}}, Data).

-spec state2(atom(), {pid(), {6}}, state_data()) -> {stop, normal, state_data()}.
state2(EventType, {BPid, {6}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state2(EventType, {BPid, {6}}, Data).

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p~n", [self()]),
    ok.

