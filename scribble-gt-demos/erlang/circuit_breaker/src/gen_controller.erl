-module(gen_controller).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s1_start_storage/2, s1/3, send_s3_start_controller/2, s3/3, s4/3, s6/3, send_s11_service_operational/2, s11/3, send_s11_error_notice/2, send_s11_shutdown_api/2, s12/3, s15/3, send_s16_storage_restart/2, s16/3, s19/3, send_s20_shutdown_storage/2, s20/3, send_s8_timeout_notice/2, s8/3]).

-include("controller.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, api_pid :: pid() | undefined, user_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
-callback s4(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s6, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s11, state_data()} | {next_state, s11, state_data(), [term()]}.
-callback s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
-callback s20(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s11(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s12, state_data()} | {next_state, s15, state_data()} | {next_state, s19, state_data()} | {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {keep_state, state_data()}.
-callback s12(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {next_state, s6, state_data()} | {keep_state, state_data()}.
-callback s15(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s16, state_data(), [{next_event, internal, {storage_restart}}]} | {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {keep_state, state_data()}.
-callback s16(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
-callback s19(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s20, state_data(), [{next_event, internal, {shutdown_storage}}]} | {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {keep_state, state_data()}.
-callback s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {start_controller}}]}.
-callback init(Args :: list()) -> {ok, s1, state_data(), [{next_event, internal, {start_storage}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_controller, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "controller_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data(), [{next_event, internal, {start_storage}}]}.
init({CallbackModule, _Args}) ->
    io:format("controller: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
s3(EventType, {start_controller}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {start_controller}, Data).

-spec s4(term(), {pid(), {atom()}}, state_data()) -> {next_state, s6, state_data()}.
s4(EventType, {StoragePid, {hard_ping}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {StoragePid, {hard_ping}}, Data).

-spec send_s11_error_notice(APIPid :: pid(), Data :: state_data()) -> ok.
send_s11_error_notice(APIPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APIPid, {self(), {error_notice}, Counter}).

-spec s6(term(), {pid(), {atom()}}, state_data()) -> {next_state, s11, state_data()} | {next_state, s11, state_data(), [term()]}.
s6(EventType, {APIPid, {get_mode}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {APIPid, {get_mode}}, Data).

-spec send_s11_service_operational(APIPid :: pid(), Data :: state_data()) -> ok.
send_s11_service_operational(APIPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APIPid, {self(), {service_operational}, Counter}).

-spec s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
s8(EventType, {timeout_notice}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {timeout_notice}, Data).

-spec send_s20_shutdown_storage(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s20_shutdown_storage(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {shutdown_storage}, Counter}).

-spec send_s1_start_storage(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s1_start_storage(StoragePid, _Data) ->
    gen_statem:cast(StoragePid, {self(), {start_storage}}).

-spec s20(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s20(EventType, {shutdown_storage}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s20(EventType, {shutdown_storage}, Data).

-spec send_s16_storage_restart(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s16_storage_restart(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {storage_restart}, Counter}).

-spec s11(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s12, state_data()} | {next_state, s15, state_data()} | {next_state, s19, state_data()} | {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {keep_state, state_data()}.
s11(EventType, {service_operational}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {service_operational}, NewData);
s11(EventType, {error_notice}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {error_notice}, NewData);
s11(EventType, {shutdown_api}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {shutdown_api}, NewData);
s11(EventType, {APIPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {APIPid, {timeout}}, NewData);
s11(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
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

-spec s12(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {next_state, s6, state_data()} | {keep_state, state_data()}.
s12(EventType, {APIPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {APIPid, {timeout}}, Data);
s12(EventType, {APIPid, {ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {APIPid, {ack}}, Data);
s12(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
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

-spec s15(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s16, state_data(), [{next_event, internal, {storage_restart}}]} | {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {keep_state, state_data()}.
s15(EventType, {APIPid, {error_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {APIPid, {error_ack}}, Data);
s15(EventType, {APIPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {APIPid, {timeout}}, Data);
s15(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
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

-spec send_s11_shutdown_api(APIPid :: pid(), Data :: state_data()) -> ok.
send_s11_shutdown_api(APIPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APIPid, {self(), {shutdown_api}, Counter}).

-spec s16(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
s16(EventType, {storage_restart}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s16(EventType, {storage_restart}, Data).

-spec s19(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s20, state_data(), [{next_event, internal, {shutdown_storage}}]} | {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} | {keep_state, state_data()}.
s19(EventType, {APIPid, {shutdown_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {APIPid, {shutdown_ack}}, Data);
s19(EventType, {APIPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {APIPid, {timeout}}, Data);
s19(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
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

-spec send_s3_start_controller(APIPid :: pid(), Data :: state_data()) -> ok.
send_s3_start_controller(APIPid, _Data) ->
    gen_statem:cast(APIPid, {self(), {start_controller}}).

-spec send_s8_timeout_notice(StoragePid :: pid(), Data :: state_data()) -> ok.
send_s8_timeout_notice(StoragePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(StoragePid, {self(), {timeout_notice}, Counter}).

-spec s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {start_controller}}]}.
s1(EventType, {start_storage}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {start_storage}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

