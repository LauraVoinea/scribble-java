-module(gen_c).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s19/3, s20/3]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, b_pid :: pid() | undefined}.

-callback s20(EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s19(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s20, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> gen_statem:callback_mode().
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s19, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("c: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s20(EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s20(EventType, {APid, {a6}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter == MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s20(EventType, {APid, {a6}}, Data);
s20(_EventType, {_Pid, _Label, _Counter}, Data) ->
    {keep_state, Data}.

-spec s19(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s20, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s19(EventType, {a2}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {a2}, NewData);
s19(EventType, {BPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter == MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {BPid, {'Timeout'}}, Data);
s19(_EventType, {_Pid, _Label, _Counter}, Data) ->
    {keep_state, Data}.

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

