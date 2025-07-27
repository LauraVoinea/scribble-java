-module(gen_p).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s4_Start/2, 
	 s4/3, 
	 send_s6_Stop/2, 
	 s6/3, 
	 send_s6_More/2, 
	 s9/3
	 ]).

-include("p.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), q_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s6, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s6(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {next_state, s9, state_data()} | {next_state, s6, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s9(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data(), [{next_event, internal, {'Start'}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_p, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "p_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data(), [{next_event, internal, {'Start'}}]}.
init({CallbackModule, _Args}) ->
    io:format("p: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s4_Start(QPid :: pid(), Data :: state_data()) -> ok.
send_s4_Start(QPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(QPid, {self(), {'Start'}, Counter}).

-spec s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s6, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s4(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_p: Postponing event ~p~n", [['Ack']]),
    {keep_state, Data, [postpone]};
s4(EventType, {'Start'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {'Start'}, NewData);
s4(EventType, {QPid, {'Interrupt'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {QPid, {'Interrupt'}}, NewData);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Ack'} 
		orelse Msg =:= {'Interrupt'} ->
    io:format("gen_p: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s6_Stop(QPid :: pid(), Data :: state_data()) -> ok.
send_s6_Stop(QPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(QPid, {self(), {'Stop'}, Counter}).

-spec s6(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s9, state_data()} |
    {next_state, s6, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s6(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_p: Postponing event ~p~n", [['Ack']]),
    {keep_state, Data, [postpone]};
s6(EventType, {'Stop'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {'Stop'}, Data);
s6(EventType, {'More'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {'More'}, Data);
s6(EventType, {QPid, {'Interrupt'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {QPid, {'Interrupt'}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Ack'} ->
    io:format("gen_p: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s9(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s9(EventType, {QPid, {'Ack'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {QPid, {'Ack'}}, Data);
s9(EventType, {QPid, {'Interrupt'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {QPid, {'Interrupt'}}, Data);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Ack'} ->
    io:format("gen_p: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s6_More(QPid :: pid(), Data :: state_data()) -> ok.
send_s6_More(QPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(QPid, {self(), {'More'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

