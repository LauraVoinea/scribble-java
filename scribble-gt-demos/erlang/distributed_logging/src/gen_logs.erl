-module(gen_logs).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 send_s9_log_success/3, 
	 s9/3, 
	 send_s9_log_failure/3, 
	 s10/3, 
	 s13/3, 
	 send_s5_ack/2, 
	 s5/3, 
	 s6/3
	 ]).

-include("logs.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), controller_pid :: pid() | undefined}.

-callback s5(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} | {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]} | {keep_state, state_data()}.
-callback s10(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s5, state_data(), [{next_event, internal, {ack}}]} | {keep_state, state_data()} | {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} | {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]}.
-callback s13(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} | {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]} | {keep_state, state_data()} | {next_state, s5, state_data(), [{next_event, internal, {ack}}]} | {stop, normal, state_data()}.
-callback s9(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s10, state_data()} | {next_state, s13, state_data()} | {keep_state, state_data(), [postpone]} | {next_state, s5, state_data(), [{next_event, internal, {ack}}]} | {keep_state, state_data()}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} | {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]} | {keep_state, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_logs, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "logs_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("logs: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s5(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
s5(EventType, {ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {ack}, Data).

-spec s6(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]} |
    {keep_state, state_data()}.
s6(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {success_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[success_ack]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {stop_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[stop_logging, Int]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {restart_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart_logging, Int]]),
    {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {restart, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart, Int]]),
    {keep_state, Data, [postpone]};
s6(EventType, {ControllerPid, {restart, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {ControllerPid, {restart, Int}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {success_ack} ->
    io:format("gen_logs: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s6(_EventType, {_Pid, {restart, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart, Int}]),
    {keep_state, Data};
s6(_EventType, {_Pid, {restart_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart_logging, Int}]),
    {keep_state, Data};
s6(_EventType, {_Pid, {stop_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{stop_logging, Int}]),
    {keep_state, Data}.

-spec s10(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s5, state_data(), [{next_event, internal, {ack}}]} |
    {keep_state, state_data()} |
    {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]}.
s10(_EventType, {_Pid, {restart, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart, Int]]),
    {keep_state, Data, [postpone]};
s10(_EventType, {_Pid, {stop_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[stop_logging, Int]]),
    {keep_state, Data, [postpone]};
s10(_EventType, {_Pid, {restart_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart_logging, Int]]),
    {keep_state, Data, [postpone]};
s10(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s10(_EventType, {_Pid, {success_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[success_ack]]),
    {keep_state, Data, [postpone]};
s10(EventType, {ControllerPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {ControllerPid, {timeout}}, Data);
s10(EventType, {ControllerPid, {success_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {ControllerPid, {success_ack}}, Data);
s10(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {success_ack} ->
    io:format("gen_logs: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s10(_EventType, {_Pid, {restart, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart, Int}]),
    {keep_state, Data};
s10(_EventType, {_Pid, {restart_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart_logging, Int}]),
    {keep_state, Data};
s10(_EventType, {_Pid, {stop_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{stop_logging, Int}]),
    {keep_state, Data}.

-spec send_s9_log_failure(ControllerPid :: pid(), Int :: term(), Data :: state_data()) -> ok.
send_s9_log_failure(ControllerPid, Int, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {log_failure, Int}, Counter}).

-spec send_s5_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s5_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {ack}, Counter}).

-spec s13(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]} |
    {keep_state, state_data()} |
    {next_state, s5, state_data(), [{next_event, internal, {ack}}]} |
    {stop, normal, state_data()}.
s13(_EventType, {_Pid, {success_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[success_ack]]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {restart, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart, Int]]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {stop_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[stop_logging, Int]]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {restart_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart_logging, Int]]),
    {keep_state, Data, [postpone]};
s13(EventType, {ControllerPid, {restart_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {ControllerPid, {restart_logging, Int}}, Data);
s13(EventType, {ControllerPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {ControllerPid, {timeout}}, Data);
s13(EventType, {ControllerPid, {stop_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {ControllerPid, {stop_logging, Int}}, Data);
s13(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {success_ack} ->
    io:format("gen_logs: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s13(_EventType, {_Pid, {restart, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart, Int}]),
    {keep_state, Data};
s13(_EventType, {_Pid, {restart_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart_logging, Int}]),
    {keep_state, Data};
s13(_EventType, {_Pid, {stop_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{stop_logging, Int}]),
    {keep_state, Data}.

-spec s9(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s10, state_data()} |
    {next_state, s13, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {next_state, s5, state_data(), [{next_event, internal, {ack}}]} |
    {keep_state, state_data()}.
s9(_EventType, {_Pid, {success_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[success_ack]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {stop_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[stop_logging, Int]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {restart, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart, Int]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {restart_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart_logging, Int]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_logs: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s9(EventType, {log_success}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {log_success}, NewData);
s9(EventType, {log_failure}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {log_failure}, NewData);
s9(EventType, {ControllerPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {ControllerPid, {timeout}}, NewData);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {timeout} 
		orelse Msg =:= {success_ack} ->
    io:format("gen_logs: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s9(_EventType, {_Pid, {restart, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart, Int}]),
    {keep_state, Data};
s9(_EventType, {_Pid, {restart_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{restart_logging, Int}]),
    {keep_state, Data};
s9(_EventType, {_Pid, {stop_logging, Int}, _Counter}, Data) ->
    io:format("gen_logs: Garbage collecting event ~p~n", [{stop_logging, Int}]),
    {keep_state, Data}.

-spec send_s9_log_success(ControllerPid :: pid(), Int :: term(), Data :: state_data()) -> ok.
send_s9_log_success(ControllerPid, Int, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {log_success, Int}, Counter}).

-spec s1(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_success}}]} |
    {next_state, s9, state_data(), [{next_event, internal, {log_failure}}]} |
    {keep_state, state_data()}.
s1(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {success_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[success_ack]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {stop_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[stop_logging, Int]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {restart, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart, Int]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {restart_logging, Int}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_logs: Postponing event ~p~n", [[restart_logging, Int]]),
    {keep_state, Data, [postpone]};
s1(EventType, {ControllerPid, {start_logging, Int}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {ControllerPid, {start_logging, Int}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

