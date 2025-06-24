-module(gen_storage).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s1/3, send_s3_hard_ping/2, s3/3, s8/3, send_s9_storage_reponse/2, s9/3, s12/3, s15/3]).

-include("storage.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), api_pid :: pid() | undefined, user_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
-callback s12(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s8, state_data()} | {keep_state, state_data()}.
-callback s8(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s9, state_data(), [{next_event, internal, {storage_reponse}}]} | {next_state, s12, state_data()} | {next_state, s15, state_data()} | {next_state, s8, state_data()} | {keep_state, state_data()}.
-callback s15(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {hard_ping}}]}.
-callback init(Args :: list()) -> {ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_storage, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "storage_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("storage: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
s3(EventType, {hard_ping}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {hard_ping}, Data).

-spec send_s3_hard_ping(ControllerPid :: pid(), Data :: state_data()) -> ok.
send_s3_hard_ping(ControllerPid, _Data) ->
    gen_statem:cast(ControllerPid, {self(), {hard_ping}}).

-spec s12(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s8, state_data()} | {keep_state, state_data()}.
s12(EventType, {ControllerPid, {storage_restart}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {ControllerPid, {storage_restart}}, Data);
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

-spec s8(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s9, state_data(), [{next_event, internal, {storage_reponse}}]} | {next_state, s12, state_data()} | {next_state, s15, state_data()} | {next_state, s8, state_data()} | {keep_state, state_data()}.
s8(EventType, {APIPid, {storage_request}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {storage_request}}, NewData);
s8(EventType, {APIPid, {cancel_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {cancel_ack}}, NewData);
s8(EventType, {APIPid, {prepare_shutdown}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {prepare_shutdown}}, NewData);
s8(EventType, {ControllerPid, {timeout_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1->
	NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {ControllerPid, {timeout_notice}}, NewData);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
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

-spec s15(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s15(EventType, {ControllerPid, {shutdown_storage}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {ControllerPid, {shutdown_storage}}, Data);
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

-spec s9(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
s9(EventType, {storage_reponse}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {storage_reponse}, Data).

-spec send_s9_storage_reponse(APIPid :: pid(), Data :: state_data()) -> ok.
send_s9_storage_reponse(APIPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APIPid, {self(), {storage_reponse}, Counter}).

-spec s1(term(), {pid(), {atom()}}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {hard_ping}}]}.
s1(EventType, {ControllerPid, {start_storage}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {ControllerPid, {start_storage}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

