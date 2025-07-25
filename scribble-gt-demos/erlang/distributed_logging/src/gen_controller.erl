-module(gen_controller).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s1_start_logging/3, 
	 s1/3, 
	 send_s9_timeout/2, 
	 s9/3, 
	 send_s10_success_ack/2, 
	 s10/3, 
	 send_s13_restart_logging/3, 
	 s13/3, 
	 send_s13_stop_logging/3, 
	 s5/3, 
	 send_s6_restart/3, 
	 s6/3
	 ]).

-include("controller.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), logs_pid :: pid() | undefined}.

-callback s13(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {start_logging}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_controller, {CallbackModule, Args},
              [{debug, [trace, {log_to_file, "controller_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {start_logging}}]}.
init({CallbackModule, _Args}) ->
    io:format("controller: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s6_restart(LogsPid :: pid(), Int :: term(), Data :: state_data()) -> ok.
send_s6_restart(LogsPid, Int, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(LogsPid, {self(), {restart, Int}, Counter}).

s5(_EventType, {_Pid, {log_success, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_controller: Postponing event ~p~n", [[log_success, Int]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {log_failure, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_controller: Postponing event ~p~n", [[log_failure, Int]]),
    {keep_state, Data, [postpone]};
s5(EventType, {LogsPid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {LogsPid, {ack}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {ack} ->
    io:format("gen_controller: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s5(_EventType, {_Pid, {log_success, Int}, _Counter}, Data) ->
    io:format("gen_controller: Garbage collecting event ~p~n", [{log_success, Int}]),
    {keep_state, Data};
s5(_EventType, {_Pid, {log_failure, Int}, _Counter}, Data) ->
    io:format("gen_controller: Garbage collecting event ~p~n", [{log_failure, Int}]),
    {keep_state, Data}.

s6(EventType, {restart}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {restart}, Data).

-spec send_s13_restart_logging(LogsPid :: pid(), Int :: term(), Data :: state_data()) -> ok.
send_s13_restart_logging(LogsPid, Int, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(LogsPid, {self(), {restart_logging, Int}, Counter}).

s10(EventType, {success_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {success_ack}, Data).

-spec s13(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s13(EventType, {restart_logging}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {restart_logging}, Data);
s13(EventType, {stop_logging}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {stop_logging}, Data).

-spec send_s9_timeout(LogsPid :: pid(), Data :: state_data()) -> ok.
send_s9_timeout(LogsPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(LogsPid, {self(), {timeout}, Counter}).

-spec s9(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s9(EventType, {timeout}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {timeout}, NewData);
s9(_EventType, {_Pid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_controller: Postponing event ~p~n", [[ack]]),
    {keep_state, Data, [postpone]};
s9(EventType, {LogsPid, {log_failure, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {LogsPid, {log_failure, Int}}, Data);
s9(_EventType, {_Pid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_controller: Postponing event ~p~n", [[ack]]),
    {keep_state, Data, [postpone]};
s9(EventType, {LogsPid, {log_success, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {LogsPid, {log_success, Int}}, Data);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {ack} ->
    io:format("gen_controller: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s9(_EventType, {_Pid, {log_success, Int}, _Counter}, Data) ->
    io:format("gen_controller: Garbage collecting event ~p~n", [{log_success, Int}]),
    {keep_state, Data};
s9(_EventType, {_Pid, {log_failure, Int}, _Counter}, Data) ->
    io:format("gen_controller: Garbage collecting event ~p~n", [{log_failure, Int}]),
    {keep_state, Data}.

-spec send_s1_start_logging(LogsPid :: pid(), Int :: term(), _Data :: state_data()) -> ok.
send_s1_start_logging(LogsPid, Int, _Data) ->
    gen_statem:cast(LogsPid, {self(), {start_logging, Int}}).

-spec send_s10_success_ack(LogsPid :: pid(), Data :: state_data()) -> ok.
send_s10_success_ack(LogsPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(LogsPid, {self(), {success_ack}, Counter}).

-spec send_s13_stop_logging(LogsPid :: pid(), Int :: term(), Data :: state_data()) -> ok.
send_s13_stop_logging(LogsPid, Int, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(LogsPid, {self(), {stop_logging, Int}, Counter}).

s1(EventType, {start_logging}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {start_logging}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

