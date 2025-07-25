-module(gen_c).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s4/3, 
	 s5/3, 
	 send_s6_a5/2, 
	 s6/3
	 ]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, b_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
-callback s5(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args},
              [{debug, [trace, {log_to_file, "c_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("c: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s6_a5(APid :: pid(), Data :: state_data()) -> ok.
send_s6_a5(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {a5}, Counter}).

-spec s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
s4(EventType, {APid, {a2}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {APid, {a2}}, NewData);
s4(EventType, {BPid, {'TOc'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {BPid, {'TOc'}}, NewData);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a2} 
		orelse Msg =:= {a3} 
		orelse Msg =:= {'TOc'} ->
    io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s5(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s5(EventType, {BPid, {'TOc'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {BPid, {'TOc'}}, Data);
s5(EventType, {BPid, {a3}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {BPid, {a3}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a3} ->
    io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(EventType, {a5}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {a5}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

