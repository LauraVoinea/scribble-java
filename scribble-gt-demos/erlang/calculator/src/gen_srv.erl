-module(gen_srv).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s1/3, s3/3, send_s6_timeout/2, s6/3, send_s7_result_sum/3, s7/3, send_s9_result_diff/3, s9/3]).

-include("srv.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-callback s3(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s6, state_data(), [{next_event, internal, {timeout}}]}.
-callback s6(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s7, state_data(), [{next_event, internal, {result_sum}}]} | {next_state, s9, state_data(), [{next_event, internal, {result_diff}}]} | {keep_state, state_data()}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s3, state_data()}.
-callback init(Args :: list()) -> {ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_srv, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "server_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("srv: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(term(), {pid(), {atom(), integer()}}, state_data()) -> {next_state, s6, state_data(), [{next_event, internal, {timeout}}]}.
s3(EventType, {CarolPid, {second, Number}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {CarolPid, {second, Number}}, Data).

-spec send_s6_timeout(CarolPid :: pid(), Data :: state_data()) -> ok.
send_s6_timeout(CarolPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CarolPid, {self(), {timeout}, Counter}).

-spec s6(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s7, state_data(), [{next_event, internal, {result_sum}}]} | {next_state, s9, state_data(), [{next_event, internal, {result_diff}}]} | {keep_state, state_data()}.
s6(EventType, {timeout}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {timeout}, NewData);
s6(EventType, {CarolPid, {sum}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {CarolPid, {sum}}, Data);
s6(EventType, {CarolPid, {diff}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {CarolPid, {diff}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {diff} 
		orelse Msg =:= {diff_result} 
		orelse Msg =:= {result_sum} 
		orelse Msg =:= {cancel} 
		orelse Msg =:= {sum} 
		orelse Msg =:= {result_diff} 
		orelse Msg =:= {sum_result} 
		orelse Msg =:= {timeout} ->
    {keep_state, Data}.

-spec s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(EventType, {result_sum}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {result_sum}, Data).

-spec s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(EventType, {result_diff}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {result_diff}, Data).

-spec send_s9_result_diff(CarolPid :: pid(), Result :: integer(), Data :: state_data()) -> ok.
send_s9_result_diff(CarolPid, Result, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CarolPid, {self(), {result_diff, Result}, Counter}).

-spec send_s7_result_sum(CarolPid :: pid(), Result :: integer(), Data :: state_data()) -> ok.
send_s7_result_sum(CarolPid, Result, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CarolPid, {self(), {result_sum, Result}, Counter}).

-spec s1(term(), {pid(), {atom(), integer()}}, state_data()) -> {next_state, s3, state_data()}.
s1(EventType, {CarolPid, {first, Number}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {CarolPid, {first, Number}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

