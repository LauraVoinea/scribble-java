-module(gen_a).
-behaviour(gen_statem).

-export([start_link/2, callback_mode/0, init/1, terminate/3]).

-export([send_1/1, state1/3, state2/3]).

-include("a.hrl").

-type state_data() :: #state_data{}.

-callback state2(atom(),{pid(), {5}} | {pid(), {exception}}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()}.

-callback state1(atom(),{1} | {pid(), {exception}}, state_data()) -> {next_state, state2, state_data()} | {stop, normal, state_data()}.

-spec start_link(module(), list()) -> {ok, pid()} | {error, any()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_a, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

callback_mode() ->
    state_functions.

-spec init({module(), list()}) -> {ok, state1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("gen_a: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).


-spec send_1(pid()) -> ok.
send_1(BPid) -> 
    gen_statem:cast(BPid, {self(), {1}}).
 
-spec state1(atom(), {1} | {pid(), {exception}}, state_data()) -> {next_state, state2, state_data()} | {stop, normal, state_data()}.
state1(EventType, {1}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state1(EventType, {1}, Data);
state1(EventType, {BPid, {exception}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state1(EventType, {BPid, {exception}}, Data).

-spec state2(atom(), {pid(), {5}} | {pid(), {exception}}, state_data()) -> {stop, normal, state_data()}.
state2(EventType, {BPid, {5}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state2(EventType, {BPid, {5}}, Data);
state2(EventType, {BPid, {exception}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state2(EventType, {BPid, {exception}}, Data).

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p~n", [self()]),
    ok.

