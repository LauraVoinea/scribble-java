-module(gen_user).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s3_request/2, s1/3, s3/3, s7/3]).

-include("usr.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, api_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s7, state_data()}.
-callback s7(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {ok, s3, state_data(), [{next_event, internal, {request}}]} | {stop, normal, state_data()} | {ok, s3, state_data(), [{next_event, internal, {request}}]} | {ok, s3, state_data(), [{next_event, internal, {request}}]} | {keep_state, state_data()}.
-callback init(Args :: list()) -> {ok, s3, state_data(), [{next_event, internal, {request}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_user, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "user_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("user: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s1(term(), {pid(), {atom()}}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {request}}]}.
s1(EventType, {APIPid, {ready}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {APIPid, {ready}}, Data).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s7, state_data()}.
s3(EventType, {request}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {request}, Data).

-spec s7(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {ok, s3, state_data(), [{next_event, internal, {request}}]} | {stop, normal, state_data()} | {ok, s3, state_data(), [{next_event, internal, {request}}]} | {ok, s3, state_data(), [{next_event, internal, {request}}]} | {keep_state, state_data()}.
s7(EventType, {APIPid, {api_response}, Counter}, #state_data{mc_counter_1 = MC} = Data)  when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {APIPid, {api_response}}, NewData);
s7(EventType, {APIPid, {shutdown_user}, Counter}, #state_data{mc_counter_1 = MC} = Data)  when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {APIPid, {shutdown_user}}, NewData);
s7(EventType, {APIPid, {error_response}, Counter}, #state_data{mc_counter_1 = MC} = Data)  when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {APIPid, {error_response}}, NewData);
s7(EventType, {APIPid, {timeout_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {APIPid, {timeout_notice}}, NewData);
s7(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {shutdown_storage} 
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

-spec send_s3_request(APIPid :: pid(), Data :: state_data()) -> ok.
send_s3_request(APIPid, _Data) ->
    gen_statem:cast(APIPid, {self(), {request}}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

