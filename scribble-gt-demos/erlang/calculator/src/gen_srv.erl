-module(gen_srv).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s14/3, s16/3, send_s19_timeout/2, s19/3, send_s20_result_sum/2, s20/3, send_s22_result_diff/2, s22/3]).

-include("srv.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-callback s20(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s22(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s14(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s16, state_data()}.
-callback s16(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s19, state_data(), [{next_event, internal, {timeout}}]}.
-callback s19(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s22, state_data(), [{next_event, internal, {result_diff}}]} | {next_state, s20, state_data(), [{next_event, internal, {result_sum}}]} | {keep_state, state_data()}.
-callback init(Args :: list()) -> {ok, s14, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_srv, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s14, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("srv: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s20(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s20(EventType, {result_sum}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s20(EventType, {result_sum}, Data).

-spec s22(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s22(EventType, {result_diff}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {result_diff}, Data).

-spec send_s19_timeout(CarolPid :: pid(), Data :: state_data()) -> ok.
send_s19_timeout(CarolPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CarolPid, {self(), {timeout}, Counter}).

-spec send_s22_result_diff(CarolPid :: pid(), Data :: state_data()) -> ok.
send_s22_result_diff(CarolPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CarolPid, {self(), {result_diff, result}, Counter}).

-spec s14(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s16, state_data()}.
s14(EventType, {CarolPid, {first, number}, Counter}, #state_data{} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {CarolPid, {first, number}}, Data).

-spec s16(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s19, state_data(), [{next_event, internal, {timeout}}]}.
s16(EventType, {CarolPid, {second, number}, Counter}, #state_data{} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s16(EventType, {CarolPid, {second, number}}, Data).

-spec s19(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s22, state_data(), [{next_event, internal, {result_diff}}]} | {next_state, s20, state_data(), [{next_event, internal, {result_sum}}]} | {keep_state, state_data()}.
s19(EventType, {timeout}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {timeout}, Data);
s19(EventType, {CarolPid, {diff}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {CarolPid, {diff}}, NewData);
s19(EventType, {CarolPid, {sum}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {CarolPid, {sum}}, NewData);
s19(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= diff 
		orelse Msg =:= diff_result 
		orelse Msg =:= result_sum 
		orelse Msg =:= cancel 
		orelse Msg =:= sum 
		orelse Msg =:= result_diff 
		orelse Msg =:= sum_result 
		orelse Msg =:= timeout ->
    {keep_state, Data}.

-spec send_s20_result_sum(CarolPid :: pid(), Data :: state_data()) -> ok.
send_s20_result_sum(CarolPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CarolPid, {self(), {result_sum, result}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

