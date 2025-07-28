-module(gen_a).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s4_a1/2, 
	 s4/3, 
	 send_s5_a2/2, 
	 s5/3, 
	 s6/3, 
	 s7/3
	 ]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s5, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {next_state, s6, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s7, state_data()} | {stop, normal, state_data()}.
-callback s7(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data(), [{next_event, internal, {a1}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_a, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "a_debug.log"}]}]);
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

-spec s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s5, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s4(_EventType, {_Pid, {a4}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
    io:format("gen_a: Postponing event ~p~n", [[a4]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
    io:format("gen_a: Postponing event ~p~n", [[a5]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {'TOa'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_a: Postponing event ~p~n", [['TOa']]),
    {keep_state, Data, [postpone]};
s4(EventType, {a1}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {a1}, NewData);
s4(EventType, {BPid, {'TOa'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {BPid, {'TOa'}}, NewData);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a4} 
		orelse Msg =:= {a5} 
		orelse Msg =:= {'TOa'} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s6, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s5(_EventType, {_Pid, {a4}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[a4]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[a5]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'TOa'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [['TOa']]),
    {keep_state, Data, [postpone]};
s5(EventType, {a2}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {a2}, Data);
s5(EventType, {BPid, {'TOa'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {BPid, {'TOa'}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a4} 
		orelse Msg =:= {a5} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s6(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s7, state_data()} |
    {stop, normal, state_data()}.
s6(_EventType, {_Pid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[a5]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {a4}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[a4]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'TOa'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [['TOa']]),
    {keep_state, Data, [postpone]};
s6(EventType, {BPid, {a4}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {BPid, {a4}}, Data);
s6(EventType, {BPid, {'TOa'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {BPid, {'TOa'}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a4} 
		orelse Msg =:= {a5} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s7(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s7(_EventType, {_Pid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[a5]]),
    {keep_state, Data, [postpone]};
s7(EventType, {CPid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {CPid, {a5}}, Data);
s7(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a5} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s5_a2(CPid :: pid(), Data :: state_data()) -> ok.
send_s5_a2(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a2}, Counter}).

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

