-module(gen_alice).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s4/3
	 ]).

-include("alice.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, srv_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {keep_state, state_data(), [postpone]}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_alice, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "alice_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("alice: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]}.
s4(_EventType, {_Pid, {cancel}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[cancel]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {sum_result, Result}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[sum_result, Result]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {diff_result, Result}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[diff_result, Result]]),
    {keep_state, Data, [postpone]};
s4(EventType, {CarolPid, {diff_result, Result}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {CarolPid, {diff_result, Result}}, NewData);
s4(EventType, {CarolPid, {sum_result, Result}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {CarolPid, {sum_result, Result}}, NewData);
s4(EventType, {CarolPid, {cancel}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {CarolPid, {cancel}}, NewData);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {cancel} ->
    io:format("gen_alice: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s4(_EventType, {_Pid, {sum_result, Result}, _Counter}, Data) ->
    io:format("gen_alice: Garbage collecting event ~p~n", [{sum_result, Result}]),
    {keep_state, Data};
s4(_EventType, {_Pid, {diff_result, Result}, _Counter}, Data) ->
    io:format("gen_alice: Garbage collecting event ~p~n", [{diff_result, Result}]),
    {keep_state, Data}.

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

