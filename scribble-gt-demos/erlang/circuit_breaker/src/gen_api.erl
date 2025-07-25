-module(gen_api).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 send_s3_ready/2, 
	 s3/3, 
	 s5/3, 
	 send_s6_get_mode/2, 
	 s6/3, 
	 send_s11_timeout/2, 
	 s11/3, 
	 send_s12_ack/2, 
	 s12/3, 
	 send_s13_storage_request/2, 
	 s13/3, 
	 s14/3, 
	 send_s15_api_response/2, 
	 s15/3, 
	 send_s18_error_ack/2, 
	 s18/3, 
	 send_s19_cancel_ack/2, 
	 s19/3, 
	 send_s20_error_response/2, 
	 s20/3, 
	 send_s23_shutdown_ack/2, 
	 s23/3, 
	 send_s24_prepare_shutdown/2, 
	 s24/3, 
	 send_s25_shutdown_user/2, 
	 s25/3, 
	 send_s8_timeout_notice/2, 
	 s8/3
	 ]).

-include("api.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, user_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-callback s13(EventType :: term(), {atom()}, state_data()) -> {next_state, s14, state_data()}.
-callback s15(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
-callback s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
-callback s20(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
-callback s25(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
%%            gen_statem:start_link({local, CallbackModule}, gen_api, {CallbackModule, Args}, []);
          gen_statem:start_link({local, CallbackModule}, gen_api, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "api_debug.log"}]}]);
      {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("api: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s19_cancel_ack(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s19_cancel_ack(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {cancel_ack}, Counter}).

s11(_EventType, {_Pid, {storage_reponse}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[storage_reponse]]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {request}}, Data) ->
    io:format("gen_api: Postponing event ~p~n", [[request]]),
    {keep_state, Data, [postpone]};
s11(EventType, {timeout}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {timeout}, NewData);
s11(EventType, {ControllerPid, {error_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {ControllerPid, {error_notice}}, Data);
s11(EventType, {ControllerPid, {shutdown_api}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {ControllerPid, {shutdown_api}}, Data);
s11(EventType, {ControllerPid, {service_operational}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {ControllerPid, {service_operational}}, Data);
s11(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {error_notice} 
		orelse Msg =:= {storage_reponse} 
		orelse Msg =:= {shutdown_api} 
		orelse Msg =:= {service_operational} ->
    io:format("gen_api: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s13(EventType :: term(), {atom()}, state_data()) -> {next_state, s14, state_data()}.
s13(EventType, {storage_request}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {storage_request}, Data).

s12(EventType, {ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {ack}, Data).

-spec s15(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s15(EventType, {api_response}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {api_response}, Data).

s14(_EventType, {_Pid, {shutdown_api}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[shutdown_api]]),
    {keep_state, Data, [postpone]};
s14(_EventType, {_Pid, {error_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[error_notice]]),
    {keep_state, Data, [postpone]};
s14(_EventType, {_Pid, {request}}, Data) ->
    io:format("gen_api: Postponing event ~p~n", [[request]]),
    {keep_state, Data, [postpone]};
s14(_EventType, {_Pid, {service_operational}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[service_operational]]),
    {keep_state, Data, [postpone]};
s14(EventType, {StoragePid, {storage_reponse}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {StoragePid, {storage_reponse}}, Data);
s14(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {error_notice} 
		orelse Msg =:= {storage_reponse} 
		orelse Msg =:= {shutdown_api} 
		orelse Msg =:= {service_operational} ->
    io:format("gen_api: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

s19(EventType, {cancel_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {cancel_ack}, Data).

s18(EventType, {error_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {error_ack}, Data).

-spec send_s20_error_response(UserPid :: pid(), Data :: state_data()) -> ok.
send_s20_error_response(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {error_response}, Counter}).

-spec send_s11_timeout(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s11_timeout(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {timeout}, Counter}).

-spec send_s8_timeout_notice(UserPid :: pid(), Data :: state_data()) -> ok.
send_s8_timeout_notice(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {timeout_notice}, Counter}).

s1(_EventType, {_Pid, {shutdown_api}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[shutdown_api]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {error_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[error_notice]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {storage_reponse}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[storage_reponse]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {request}}, Data) ->
    io:format("gen_api: Postponing event ~p~n", [[request]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {service_operational}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[service_operational]]),
    {keep_state, Data, [postpone]};
s1(EventType, {ControllerPid, {start_controller}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {ControllerPid, {start_controller}}, Data).

-spec send_s25_shutdown_user(UserPid :: pid(), Data :: state_data()) -> ok.
send_s25_shutdown_user(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {shutdown_user}, Counter}).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s3(EventType, {ready}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {ready}, Data).

-spec send_s24_prepare_shutdown(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s24_prepare_shutdown(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {prepare_shutdown}, Counter}).

s5(_EventType, {_Pid, {shutdown_api}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[shutdown_api]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {error_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[error_notice]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {storage_reponse}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[storage_reponse]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {service_operational}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_api: Postponing event ~p~n", [[service_operational]]),
    {keep_state, Data, [postpone]};
s5(EventType, {UserPid, {request}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {UserPid, {request}}, Data).

s6(EventType, {get_mode}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {get_mode}, Data).

-spec s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s8(EventType, {timeout_notice}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {timeout_notice}, Data).

-spec send_s13_storage_request(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s13_storage_request(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {storage_request}, Counter}).

-spec s20(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s20(EventType, {error_response}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s20(EventType, {error_response}, Data).

-spec send_s15_api_response(UserPid :: pid(), Data :: state_data()) -> ok.
send_s15_api_response(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {api_response}, Counter}).

s24(EventType, {prepare_shutdown}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s24(EventType, {prepare_shutdown}, Data).

s23(EventType, {shutdown_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s23(EventType, {shutdown_ack}, Data).

-spec s25(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s25(EventType, {shutdown_user}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s25(EventType, {shutdown_user}, Data).

-spec send_s3_ready(UserPid :: pid(), _Data :: state_data()) -> ok.
send_s3_ready(UserPid, _Data) ->
    gen_statem:cast(UserPid, {self(), {ready}}).

-spec send_s23_shutdown_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s23_shutdown_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {shutdown_ack}, Counter}).

-spec send_s6_get_mode(ControllerPid :: pid(), _Data :: state_data()) -> ok.
send_s6_get_mode(ControllerPid, _Data) ->
    gen_statem:cast(ControllerPid, {self(), {get_mode}}).

-spec send_s18_error_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s18_error_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {error_ack}, Counter}).

-spec send_s12_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s12_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {ack}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

