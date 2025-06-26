-module(gen_a).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s4_a1/2, s4/3, s5/3, send_s6_a6/2, s6/3]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s5(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s6, state_data(), [{next_event, internal, {a6}}]} | {keep_state, state_data()}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data(), [{next_event, internal, {a1}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_a, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data(), [{next_event, internal, {a1}}]}.
init({CallbackModule, _Args}) ->
    io:format("a: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s4(EventType, {a1}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {a1}, NewData);
s4(EventType, {BPid, {'To'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {BPid, {'To'}}, Data);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a5} 
		orelse Msg =:= {'To'} ->
    {keep_state, Data}.

-spec send_s6_a6(CPid :: pid(), Data :: state_data()) -> ok.
send_s6_a6(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a6}, Counter}).

-spec s5(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s6, state_data(), [{next_event, internal, {a6}}]} | {keep_state, state_data()}.
s5(EventType, {BPid, {'To'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {BPid, {'To'}}, Data);
s5(EventType, {BPid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {BPid, {a5}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a5} 
		orelse Msg =:= {'To'} ->
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(EventType, {a6}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {a6}, Data).

-spec send_s4_a1(BPid :: pid(), Data :: state_data()) -> ok.
send_s4_a1(BPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BPid, {self(), {a1}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

