-module(gen_a).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s5_fibonacci/3, 
	 s5/3, 
	 send_s5_stop/2, 
	 s6/3, 
	 s9/3
	 ]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined}.

-callback s5(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s6, state_data()} | {next_state, s9, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {ok, s5, state_data()} | {next_state, s5, state_data(), [term()]}.
-callback s9(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s5, state_data()} | {next_state, s5, state_data(), [term()]}.

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
	{ok, s5, state_data()} | {next_state, s5, state_data(), [term()]}.
init({CallbackModule, _Args}) ->
    io:format("a: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s5(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s6, state_data()} |
    {next_state, s9, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s5(_EventType, {_Pid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[ack]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {fibonacci, Num}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[fibonacci, Num]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {error}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[error]]),
    {keep_state, Data, [postpone]};
s5(EventType, {fibonacci}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {fibonacci}, NewData);
s5(EventType, {stop}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {stop}, NewData);
s5(EventType, {BPid, {error}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {BPid, {error}}, NewData);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {error} 
		orelse Msg =:= {ack} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s5(_EventType, {_Pid, {fibonacci, Num}, _Counter}, Data) ->
    io:format("gen_a: Garbage collecting event ~p~n", [{fibonacci, Num}]),
    {keep_state, Data}.

-spec s6(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {ok, s5, state_data()} |
    {next_state, s5, state_data(), [term()]}.
s6(_EventType, {_Pid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[ack]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {error}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[error]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {fibonacci, Num}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[fibonacci, Num]]),
    {keep_state, Data, [postpone]};
s6(EventType, {BPid, {fibonacci, Num}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {BPid, {fibonacci, Num}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {ack} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s6(_EventType, {_Pid, {fibonacci, Num}, _Counter}, Data) ->
    io:format("gen_a: Garbage collecting event ~p~n", [{fibonacci, Num}]),
    {keep_state, Data}.

-spec send_s5_fibonacci(BPid :: pid(), Num :: term(), Data :: state_data()) -> ok.
send_s5_fibonacci(BPid, Num, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BPid, {self(), {fibonacci, Num}, Counter}).

-spec s9(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s9(_EventType, {_Pid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[ack]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {error}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[error]]),
    {keep_state, Data, [postpone]};
s9(EventType, {BPid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {BPid, {ack}}, Data);
s9(EventType, {BPid, {error}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {BPid, {error}}, Data);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {ack} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s5_stop(BPid :: pid(), Data :: state_data()) -> ok.
send_s5_stop(BPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BPid, {self(), {stop}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

