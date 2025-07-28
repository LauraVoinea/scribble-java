-module(gen_q).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s4_Interrupt/2, 
	 s4/3, 
	 s6/3, 
	 send_s9_Ack/2, 
	 s9/3
	 ]).

-include("q.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), p_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {keep_state, state_data(), [postpone]} | {next_state, s6, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s9, state_data(), [{next_event, internal, {'Ack'}}]} | {keep_state, state_data()} | {next_state, s6, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data(), [{next_event, internal, {'Interrupt'}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_q, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "q_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data(), [{next_event, internal, {'Interrupt'}}]}.
init({CallbackModule, _Args}) ->
    io:format("q: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {next_state, s6, state_data()}.
s4(_EventType, {_Pid, {'More'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_q: Postponing event ~p~n", [['More']]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {'Stop'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_q: Postponing event ~p~n", [['Stop']]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {'Start'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_q: Postponing event ~p~n", [['Start']]),
    {keep_state, Data, [postpone]};
s4(EventType, {'Interrupt'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {'Interrupt'}, NewData);
s4(EventType, {PPid, {'Start'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {PPid, {'Start'}}, Data);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Stop'} 
		orelse Msg =:= {'More'} 
		orelse Msg =:= {'Start'} ->
    io:format("gen_q: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s6(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s9, state_data(), [{next_event, internal, {'Ack'}}]} |
    {keep_state, state_data()} |
    {next_state, s6, state_data()}.
s6(_EventType, {_Pid, {'More'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_q: Postponing event ~p~n", [['More']]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'Stop'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_q: Postponing event ~p~n", [['Stop']]),
    {keep_state, Data, [postpone]};
s6(EventType, {PPid, {'Stop'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {PPid, {'Stop'}}, Data);
s6(EventType, {PPid, {'More'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {PPid, {'More'}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Stop'} 
		orelse Msg =:= {'More'} ->
    io:format("gen_q: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(EventType, {'Ack'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {'Ack'}, Data).

-spec send_s9_Ack(PPid :: pid(), Data :: state_data()) -> ok.
send_s9_Ack(PPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(PPid, {self(), {'Ack'}, Counter}).

-spec send_s4_Interrupt(PPid :: pid(), Data :: state_data()) -> ok.
send_s4_Interrupt(PPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(PPid, {self(), {'Interrupt'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

