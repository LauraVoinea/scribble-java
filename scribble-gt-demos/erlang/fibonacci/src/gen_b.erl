-module(gen_b).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s5_error/2, s5/3, send_s6_fibonacci/2, s6/3, send_s9_ack/2, s9/3]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined}.

-callback s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s9, state_data(), [{next_event, internal, {ack}}]} | {next_state, s6, state_data(), [{next_event, internal, {fibonacci}}]} | {keep_state, state_data()}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {ok, s5, state_data(), [{next_event, internal, {error}}]}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> {ok, s5, state_data(), [{next_event, internal, {error}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "b_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s5, state_data(), [{next_event, internal, {error}}]}.
init({CallbackModule, _Args}) ->
    io:format("B: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s9, state_data(), [{next_event, internal, {ack}}]} | {next_state, s6, state_data(), [{next_event, internal, {fibonacci}}]} | {keep_state, state_data()}.
s5(EventType, {error}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {error}, NewData);
s5(EventType, {APid, {stop}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {APid, {stop}}, Data);
s5(EventType, {APid, {fibonacci, Num}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {APid, {fibonacci, Num}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {ack}
    orelse Msg =:= {error}
    orelse Msg =:= {fibonacci}
    orelse Msg =:= {stop} ->
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) -> {ok, s5, state_data(), [{next_event, internal, {error}}]}.
s6(EventType, {fibonacci}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {fibonacci}, Data).

-spec s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(EventType, {ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {ack}, Data).

-spec send_s9_ack(APid :: pid(), Data :: state_data()) -> ok.
send_s9_ack(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {ack}, Counter}).

-spec send_s6_fibonacci(APid :: pid(), Data :: state_data()) -> ok.
send_s6_fibonacci(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {fibonacci, Data#state_data.curr_value}, Counter}).

-spec send_s5_error(APid :: pid(), Data :: state_data()) -> ok.
send_s5_error(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {error}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

