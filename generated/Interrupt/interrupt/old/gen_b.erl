-module(gen_b).
-behaviour(gen_statem).

-export([start_link/2, callback_mode/0, init/1, terminate/3]).

-export([send_6/1, send_5/1, send_2/1, send_exception/1, state1/3, state2/3, state3/3, state4/3, state6/3]).

-include("b.hrl").

-type state_data() :: #state_data{}.

-callback state6(atom(),{exception}, state_data()) -> {stop, normal, state_data()}.

-callback state4(atom(),{6}, state_data()) -> {stop, normal, state_data()}.

-callback state3(atom(),{5}, state_data()) -> {next_state, state4, state_data()}.

-callback state2(atom(),{2}, state_data()) -> {next_state, state3, state_data()}.

-callback state1(atom(),{pid(), {1}} |  exception_choice, state_data()) -> {next_state, state2, state_data()} | {next_state, state6, state_data()}.

-spec start_link(module(), list()) -> {ok, pid()} | {error, any()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

callback_mode() ->
    state_functions.

-spec init({module(), list()}) -> {ok, state1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("gen_b: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
%%    {ok, state1, #state_data{}}.
    CallbackModule:init([]).

-spec send_6(pid()) -> ok.
send_6(CPid) -> 
    gen_statem:cast(CPid, {self(), {6}}).
 
-spec send_5(pid()) -> ok.
send_5(APid) -> 
    gen_statem:cast(APid, {self(), {5}}).
 
-spec send_2(pid()) -> ok.
send_2(CPid) -> 
    gen_statem:cast(CPid, {self(), {2}}).
 
-spec send_exception(pid()) -> ok.
send_exception(CPid) -> 
    gen_statem:cast(CPid, {self(), {exception}}).
 
-spec state1(atom(), {pid(), {1}} |  exception_choice, state_data()) -> {next_state, state2, state_data()} | {next_state, state6, state_data()}.
state1(EventType, {APid, {1}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state1(EventType, {APid, {1}}, Data);
state1(EventType, exception_choice, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state1(EventType, exception_choice, Data).

-spec state2(atom(), {2}, state_data()) -> {next_state, state3, state_data()}.
state2(EventType, {2}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state2(EventType, {2}, Data).

-spec state3(atom(), {5}, state_data()) -> {next_state, state4, state_data()}.
state3(EventType, {5}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state3(EventType, {5}, Data).

-spec state4(atom(), {6}, state_data()) -> {stop, normal, state_data()}.
state4(EventType, {6}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state4(EventType, {6}, Data).

-spec state6(atom(), {exception}, state_data()) -> {stop, normal, state_data()}.
state6(EventType, {exception}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:state6(EventType, {exception}, Data).

terminate(_Reason, _State, _Data) ->
    io:format("Terminating ~p~n", [self()]),
    ok.

