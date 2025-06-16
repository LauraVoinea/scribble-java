-module(gen_alice).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s27/3]).

-include("alice.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, srv_pid :: pid() | undefined}.

-callback s27(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback init(Args :: list()) -> {ok, s27, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_alice, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s27, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("alice: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s27(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s27(EventType, {sum_result}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {sum_result}, NewData);
s27(EventType, {diff_result}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {diff_result}, NewData);
s27(EventType, {CarolPid, {cancel}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {CarolPid, {cancel}}, Data);
s27(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= diff 
		orelse Msg =:= diff_result 
		orelse Msg =:= result_sum 
		orelse Msg =:= cancel 
		orelse Msg =:= sum 
		orelse Msg =:= result_diff 
		orelse Msg =:= sum_result 
		orelse Msg =:= timeout ->
    {keep_state, Data}.

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

