-module(gen_api).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s1/3, s4/3, send_s5_get_mode/2, s5/3, 
        send_s10_timeout/2, s10/3, send_s11_ack/2, s11/3, send_s12_storage_request/2, s12/3, s13/3, send_s14_api_response/2, 
        s14/3, send_s17_error_ack/2, s17/3, send_s18_cancel_ack/2, s18/3, send_s19_error_response/2, s19/3, send_s22_shutdown_ack/2, 
        s22/3, send_s23_prepare_shutdown/2, s23/3, send_s24_shutdown_user/2, s24/3, send_s7_timeout_notice/2, s7/3,
        send_s3_ready/2, s3/3 
    ]).

-include("api.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, user_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-callback s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data(), [{next_event, internal, {storage_request}}]}.
-callback s10(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s7, state_data(), [{next_event, internal, {timeout_notice}}]} | {next_state, s11, state_data(), [{next_event, internal, {ack}}]} | {next_state, s22, state_data(), [{next_event, internal, {shutdown_ack}}]} | {next_state, s17, state_data(), [{next_event, internal, {error_ack}}]} | {keep_state, state_data()}.
-callback s13(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data(), [{next_event, internal, {api_response}}]} | {keep_state, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {next_state, s13, state_data()}.
-callback s14(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
-callback s17(EventType :: term(), {atom()}, state_data()) -> {next_state, s18, state_data(), [{next_event, internal, {cancel_ack}}]}.
-callback s19(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
-callback s18(EventType :: term(), {atom()}, state_data()) -> {next_state, s19, state_data(), [{next_event, internal, {error_response}}]}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s3, state_data()}.
-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
-callback s4(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {get_mode}}]}.
-callback s5(EventType :: term(), {atom()}, state_data()) -> {next_state, s10, state_data(), [{next_event, internal, {timeout}}]}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
-callback s22(EventType :: term(), {atom()}, state_data()) -> {next_state, s23, state_data(), [{next_event, internal, {prepare_shutdown}}]}.
-callback s24(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s23(EventType :: term(), {atom()}, state_data()) -> {next_state, s24, state_data(), [{next_event, internal, {shutdown_user}}]}.
-callback init(Args :: list()) -> {ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_api, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "api_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("api: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s7_timeout_notice(UserPid :: pid(), Data :: state_data()) -> ok.
send_s7_timeout_notice(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {timeout_notice}, Counter}).

-spec send_s19_error_response(UserPid :: pid(), Data :: state_data()) -> ok.
send_s19_error_response(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {error_response}, Counter}).

-spec send_s5_get_mode(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s5_get_mode(ControllerPid, _Data) ->
    gen_statem:cast(ControllerPid, {self(), {get_mode}}).

-spec send_s3_ready(UserPid :: pid(), Data :: state_data()) -> ok.
send_s3_ready(UserPid, _Data) ->
    gen_statem:cast(UserPid, {self(), {ready}}).

-spec s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data(), [{next_event, internal, {storage_request}}]}.
s11(EventType, {ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {ack}, Data).

-spec send_s17_error_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s17_error_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {error_ack}, Counter}).

-spec s10(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s7, state_data(), [{next_event, internal, {timeout_notice}}]} | {next_state, s11, state_data(), [{next_event, internal, {ack}}]} | {next_state, s22, state_data(), [{next_event, internal, {shutdown_ack}}]} | {next_state, s17, state_data(), [{next_event, internal, {error_ack}}]} | {keep_state, state_data()}.
s10(EventType, {timeout}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {timeout}, NewData);
s10(EventType, {ControllerPid, {service_operational}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {ControllerPid, {service_operational}}, Data);
s10(EventType, {ControllerPid, {shutdown_api}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {ControllerPid, {shutdown_api}}, Data);
s10(EventType, {ControllerPid, {error_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {ControllerPid, {error_notice}}, Data);
s10(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
		orelse Msg =:= {shutdown_user} 
		orelse Msg =:= {ack} 
		orelse Msg =:= {api_response} 
		orelse Msg =:= {error_notice} 
		orelse Msg =:= {shutdown_ack} 
		orelse Msg =:= {timeout} 
		orelse Msg =:= {shutdown_api} 
		orelse Msg =:= {service_operational} 
		orelse Msg =:= {error_response} 
		orelse Msg =:= {timeout_notice} 
		orelse Msg =:= {prepare_shutdown} 
		orelse Msg =:= {error_ack} 
		orelse Msg =:= {storage_request} 
		orelse Msg =:= {cancel_ack} 
		orelse Msg =:= {storage_restart} 
		orelse Msg =:= {storage_reponse} ->
    {keep_state, Data}.

-spec s13(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data(), [{next_event, internal, {api_response}}]} | {keep_state, state_data()}.
s13(EventType, {StoragePid, {storage_reponse}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {StoragePid, {storage_reponse}}, Data);
s13(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
		orelse Msg =:= {shutdown_user} 
		orelse Msg =:= {ack} 
		orelse Msg =:= {api_response} 
		orelse Msg =:= {error_notice} 
		orelse Msg =:= {shutdown_ack} 
		orelse Msg =:= {timeout} 
		orelse Msg =:= {shutdown_api} 
		orelse Msg =:= {service_operational} 
		orelse Msg =:= {error_response} 
		orelse Msg =:= {timeout_notice} 
		orelse Msg =:= {prepare_shutdown} 
		orelse Msg =:= {error_ack} 
		orelse Msg =:= {storage_request} 
		orelse Msg =:= {cancel_ack} 
		orelse Msg =:= {storage_restart} 
		orelse Msg =:= {storage_reponse} ->
    {keep_state, Data}.

-spec s12(EventType :: term(), {atom()}, state_data()) -> {next_state, s13, state_data()}.
s12(EventType, {storage_request}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {storage_request}, Data).

-spec s14(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
s14(EventType, {api_response}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {api_response}, Data).

-spec s17(EventType :: term(), {atom()}, state_data()) -> {next_state, s18, state_data(), [{next_event, internal, {cancel_ack}}]}.
s17(EventType, {error_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s17(EventType, {error_ack}, Data).

-spec s19(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
s19(EventType, {error_response}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {error_response}, Data).

-spec s18(EventType :: term(), {atom()}, state_data()) -> {next_state, s19, state_data(), [{next_event, internal, {error_response}}]}.
s18(EventType, {cancel_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {cancel_ack}, Data).

-spec s1(term(), {pid(), {atom()}}, state_data()) -> {next_state, s4, state_data()}.
s1(EventType, {ControllerPid, {start_controller}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {ControllerPid, {start_controller}}, Data).

-spec send_s10_timeout(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s10_timeout(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {timeout}, Counter}).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s3(EventType, {ready}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {ready}, Data).

-spec s4(term(), {pid(), {atom()}}, state_data()) ->
  {next_state, s5, state_data(), [{next_event, internal, {get_mode}}]} |
  {keep_state, state_data()}.
s4(EventType, {UserPid, {request}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {UserPid, {request}}, Data);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage}
  orelse Msg =:= {shutdown_user}
  orelse Msg =:= {ack}
  orelse Msg =:= {api_response}
  orelse Msg =:= {error_notice}
  orelse Msg =:= {shutdown_ack}
  orelse Msg =:= {timeout}
  orelse Msg =:= {shutdown_api}
  orelse Msg =:= {service_operational}
  orelse Msg =:= {error_response}
  orelse Msg =:= {timeout_notice}
  orelse Msg =:= {prepare_shutdown}
  orelse Msg =:= {error_ack}
  orelse Msg =:= {storage_request}
  orelse Msg =:= {cancel_ack}
  orelse Msg =:= {storage_restart}
  orelse Msg =:= {storage_reponse} ->
  {keep_state, Data}.


-spec s5(EventType :: term(), {atom()}, state_data()) -> {next_state, s10, state_data(), [{next_event, internal, {timeout}}]}.
s5(EventType, {get_mode}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {get_mode}, Data).

-spec send_s24_shutdown_user(UserPid :: pid(), Data :: state_data()) -> ok.
send_s24_shutdown_user(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {shutdown_user}, Counter}).

-spec send_s18_cancel_ack(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s18_cancel_ack(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {cancel_ack}, Counter}).

-spec s7(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
s7(EventType, {timeout_notice}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {timeout_notice}, Data).

-spec send_s23_prepare_shutdown(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s23_prepare_shutdown(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {prepare_shutdown}, Counter}).

-spec send_s11_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s11_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {ack}, Counter}).

-spec send_s14_api_response(UserPid :: pid(), Data :: state_data()) -> ok.
send_s14_api_response(UserPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(UserPid, {self(), {api_response}, Counter}).

-spec s22(EventType :: term(), {atom()}, state_data()) -> {next_state, s23, state_data(), [{next_event, internal, {prepare_shutdown}}]}.
s22(EventType, {shutdown_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {shutdown_ack}, Data).

-spec s24(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s24(EventType, {shutdown_user}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s24(EventType, {shutdown_user}, Data).

-spec s23(EventType :: term(), {atom()}, state_data()) -> {next_state, s24, state_data(), [{next_event, internal, {shutdown_user}}]}.
s23(EventType, {prepare_shutdown}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s23(EventType, {prepare_shutdown}, Data).

-spec send_s12_storage_request(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s12_storage_request(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {storage_request}, Counter}).

-spec send_s22_shutdown_ack(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s22_shutdown_ack(ControllerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ControllerPid, {self(), {shutdown_ack}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

