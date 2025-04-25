-module(gen_b).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s5_To/2, s5/3, send_s6_a2/2, s6/3, send_s7_a5/2, s7/3, send_s3_To/2, s3/3]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {'To'}}]} | {next_state, s6, state_data(), [{next_event, internal, {a2}}]} | {keep_state, state_data()}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {next_state, s7, state_data(), [{next_event, internal, {a5}}]}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> {ok, s5, state_data(), [{next_event, internal, {'To'}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s5, state_data(), [{next_event, internal, {'To'}}]}.
init({CallbackModule, _Args}) ->
    io:format("b: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(EventType, {'To'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {'To'}, Data).

-spec send_s7_a5(APid :: pid(), Data :: state_data()) -> ok.
send_s7_a5(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {a5}, Counter}).

-spec s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {'To'}}]} | {next_state, s6, state_data(), [{next_event, internal, {a2}}]} | {keep_state, state_data()}.
s5(EventType, {'To'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {'To'}, Data);
s5(EventType, {APid, {a1}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {APid, {a1}}, NewData);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= a6 
		orelse Msg =:= a5 
		orelse Msg =:= a2 
		orelse Msg =:= a1 
		orelse Msg =:= 'To' ->
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) -> {next_state, s7, state_data(), [{next_event, internal, {a5}}]}.
s6(EventType, {a2}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {a2}, Data).

-spec send_s3_To(CPid :: pid(), Data :: state_data()) -> ok.
send_s3_To(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'To'}, Counter}).

-spec s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(EventType, {a5}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {a5}, Data).

-spec send_s5_To(APid :: pid(), Data :: state_data()) -> ok.
send_s5_To(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {'To'}, Counter}).

-spec send_s6_a2(CPid :: pid(), Data :: state_data()) -> ok.
send_s6_a2(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a2}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

