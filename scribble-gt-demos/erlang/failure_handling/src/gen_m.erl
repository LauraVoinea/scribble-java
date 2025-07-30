-module(gen_m).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s1_init/3, 
	 s1/3, 
	 s6/3, 
	 s7/3, 
	 send_s8_more/3, 
	 s8/3
	 ]).

-include("m.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), w_pid :: pid() | undefined, fd_pid :: pid() | undefined}.

-callback s6(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s7, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s7(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s8, state_data(), [{next_event, internal, {more}}]} | {keep_state, state_data()}.
-callback s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
-callback s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {init}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_m, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "m_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {init}}]}.
init({CallbackModule, _Args}) ->
    io:format("m: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s8_more(WPid :: pid(), Payload :: term(), Data :: state_data()) -> ok.
send_s8_more(WPid, Payload, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(WPid, {self(), {more, Payload}, Counter}).

-spec s6(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s7, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s6(_EventType, {_Pid, {result, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
    io:format("gen_m: Postponing event ~p~n", [[result, Payload]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'OK'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_m: Postponing event ~p~n", [['OK']]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'Crash'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_m: Postponing event ~p~n", [['Crash']]),
    {keep_state, Data, [postpone]};
s6(EventType, {FDPid, {'OK'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {FDPid, {'OK'}}, NewData);
s6(EventType, {FDPid, {'Crash'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {FDPid, {'Crash'}}, NewData);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'OK'} 
		orelse Msg =:= {'Crash'} ->
    io:format("gen_m: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s6(_EventType, {_Pid, {result, Payload}, _Counter}, Data) ->
    io:format("gen_m: Garbage collecting event ~p~n", [{result, Payload}]),
    {keep_state, Data}.

-spec s7(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s8, state_data(), [{next_event, internal, {more}}]} |
    {keep_state, state_data()}.
s7(_EventType, {_Pid, {'OK'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_m: Postponing event ~p~n", [['OK']]),
    {keep_state, Data, [postpone]};
s7(_EventType, {_Pid, {'Crash'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_m: Postponing event ~p~n", [['Crash']]),
    {keep_state, Data, [postpone]};
s7(_EventType, {_Pid, {result, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_m: Postponing event ~p~n", [[result, Payload]]),
    {keep_state, Data, [postpone]};
s7(EventType, {WPid, {result, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {WPid, {result, Payload}}, Data);
s7(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'OK'} ->
    io:format("gen_m: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s7(_EventType, {_Pid, {result, Payload}, _Counter}, Data) ->
    io:format("gen_m: Garbage collecting event ~p~n", [{result, Payload}]),
    {keep_state, Data}.

-spec s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
s8(EventType, {more}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {more}, Data).

-spec send_s1_init(WPid :: pid(), Payload :: term(), _Data :: state_data()) -> ok.
send_s1_init(WPid, Payload, _Data) ->
    gen_statem:cast(WPid, {self(), {init, Payload}}).

-spec s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
s1(EventType, {init}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {init}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

