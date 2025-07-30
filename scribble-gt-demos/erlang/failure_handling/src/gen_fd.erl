-module(gen_fd).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s6_Timeout/2, 
	 s6/3, 
	 send_s7_OK/2, 
	 s7/3, 
	 send_s4_Crash/2, 
	 s4/3
	 ]).

-include("fd.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), w_pid :: pid() | undefined, m_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s6(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {'Crash'}}]} | {keep_state, state_data()} | {keep_state, state_data(), [postpone]} | {next_state, s7, state_data(), [{next_event, internal, {'OK'}}]}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {ok, s6, state_data(), [{next_event, internal, {'Timeout'}}]}.
-callback init(Args :: list()) -> 
	{ok, s6, state_data(), [{next_event, internal, {'Timeout'}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_fd, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "fd_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s6, state_data(), [{next_event, internal, {'Timeout'}}]}.
init({CallbackModule, _Args}) ->
    io:format("fd: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s4(EventType, {'Crash'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {'Crash'}, Data).

-spec s6(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s4, state_data(), [{next_event, internal, {'Crash'}}]} |
    {keep_state, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {next_state, s7, state_data(), [{next_event, internal, {'OK'}}]}.
s6(_EventType, {_Pid, {'HB'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_fd: Postponing event ~p~n", [['HB']]),
    {keep_state, Data, [postpone]};
s6(EventType, {'Timeout'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {'Timeout'}, NewData);
s6(EventType, {WPid, {'HB'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {WPid, {'HB'}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'HB'} ->
    io:format("gen_fd: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s7(EventType :: term(), {atom()}, state_data()) -> {ok, s6, state_data(), [{next_event, internal, {'Timeout'}}]}.
s7(EventType, {'OK'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {'OK'}, Data).

-spec send_s4_Crash(MPid :: pid(), Data :: state_data()) -> ok.
send_s4_Crash(MPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(MPid, {self(), {'Crash'}, Counter}).

-spec send_s7_OK(MPid :: pid(), Data :: state_data()) -> ok.
send_s7_OK(MPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(MPid, {self(), {'OK'}, Counter}).

-spec send_s6_Timeout(WPid :: pid(), Data :: state_data()) -> ok.
send_s6_Timeout(WPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(WPid, {self(), {'Timeout'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

