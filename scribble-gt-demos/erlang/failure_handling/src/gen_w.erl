-module(gen_w).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 send_s6_HB/2, 
	 s6/3, 
	 send_s7_result/3, 
	 s7/3, 
	 s8/3
	 ]).

-include("w.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), m_pid :: pid() | undefined, fd_pid :: pid() | undefined}.

-callback s6(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s7, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s7(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {next_state, s8, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s8(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s6, state_data(), [{next_event, internal, {'HB'}}]} | {keep_state, state_data()} | {stop, normal, state_data()}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s6, state_data(), [{next_event, internal, {'HB'}}]} | {keep_state, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_w, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "w_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("w: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s6(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s7, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s6(_EventType, {_Pid, {more, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
    io:format("gen_w: Postponing event ~p~n", [[more, Payload]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_w: Postponing event ~p~n", [['Timeout']]),
    {keep_state, Data, [postpone]};
s6(EventType, {'HB'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {'HB'}, NewData);
s6(EventType, {FDPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {FDPid, {'Timeout'}}, NewData);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Timeout'} ->
    io:format("gen_w: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s6(_EventType, {_Pid, {more, Payload}, _Counter}, Data) ->
    io:format("gen_w: Garbage collecting event ~p~n", [{more, Payload}]),
    {keep_state, Data}.

-spec s7(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s8, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s7(_EventType, {_Pid, {more, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_w: Postponing event ~p~n", [[more, Payload]]),
    {keep_state, Data, [postpone]};
s7(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_w: Postponing event ~p~n", [['Timeout']]),
    {keep_state, Data, [postpone]};
s7(EventType, {result}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {result}, Data);
s7(EventType, {FDPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {FDPid, {'Timeout'}}, Data);
s7(_EventType, {_Pid, {more, Payload}, _Counter}, Data) ->
    io:format("gen_w: Garbage collecting event ~p~n", [{more, Payload}]),
    {keep_state, Data}.

-spec s8(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s6, state_data(), [{next_event, internal, {'HB'}}]} |
    {keep_state, state_data()} |
    {stop, normal, state_data()}.
s8(_EventType, {_Pid, {more, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_w: Postponing event ~p~n", [[more, Payload]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_w: Postponing event ~p~n", [['Timeout']]),
    {keep_state, Data, [postpone]};
s8(EventType, {MPid, {more, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {MPid, {more, Payload}}, Data);
s8(EventType, {FDPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {FDPid, {'Timeout'}}, Data);
s8(_EventType, {_Pid, {more, Payload}, _Counter}, Data) ->
    io:format("gen_w: Garbage collecting event ~p~n", [{more, Payload}]),
    {keep_state, Data}.

-spec send_s6_HB(FDPid :: pid(), Data :: state_data()) -> ok.
send_s6_HB(FDPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(FDPid, {self(), {'HB'}, Counter}).

-spec send_s7_result(MPid :: pid(), Payload :: term(), Data :: state_data()) -> ok.
send_s7_result(MPid, Payload, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(MPid, {self(), {result, Payload}, Counter}).

-spec s1(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s6, state_data(), [{next_event, internal, {'HB'}}]} |
    {keep_state, state_data()}.
s1(_EventType, {_Pid, {more, Payload}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_w: Postponing event ~p~n", [[more, Payload]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_w: Postponing event ~p~n", [['Timeout']]),
    {keep_state, Data, [postpone]};
s1(EventType, {MPid, {init, Payload}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {MPid, {init, Payload}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

