-module(gen_usr).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 send_s4_request/2, 
	 s4/3, 
	 s8/3
	 ]).

-include("usr.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, api_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
-callback s8(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {request}}]} | {keep_state, state_data()} | {stop, normal, state_data()} | {keep_state, state_data(), [postpone]}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s4, state_data(), [{next_event, internal, {request}}]} | {keep_state, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_usr, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "usr_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("usr: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
s4(EventType, {request}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {request}, Data).

-spec s8(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s4, state_data(), [{next_event, internal, {request}}]} |
    {keep_state, state_data()} |
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]}.
s8(_EventType, {_Pid, {api_response}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_usr: Postponing event ~p~n", [[api_response]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {timeout_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_usr: Postponing event ~p~n", [[timeout_notice]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {error_response}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_usr: Postponing event ~p~n", [[error_response]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {shutdown_user}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_usr: Postponing event ~p~n", [[shutdown_user]]),
    {keep_state, Data, [postpone]};
s8(EventType, {APIPid, {error_response}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {error_response}}, NewData);
s8(EventType, {APIPid, {api_response}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {api_response}}, NewData);
s8(EventType, {APIPid, {shutdown_user}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {shutdown_user}}, NewData);
s8(EventType, {APIPid, {timeout_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {APIPid, {timeout_notice}}, NewData);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {error_response} 
		orelse Msg =:= {timeout_notice} 
		orelse Msg =:= {api_response} 
		orelse Msg =:= {shutdown_user} ->
    io:format("gen_usr: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s4_request(APIPid :: pid(), _Data :: state_data()) -> ok.
send_s4_request(APIPid, _Data) ->
    gen_statem:cast(APIPid, {self(), {request}}).

-spec s1(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s4, state_data(), [{next_event, internal, {request}}]} |
    {keep_state, state_data()}.
s1(_EventType, {_Pid, {api_response}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_usr: Postponing event ~p~n", [[api_response]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {timeout_notice}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_usr: Postponing event ~p~n", [[timeout_notice]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {error_response}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_usr: Postponing event ~p~n", [[error_response]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {shutdown_user}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_usr: Postponing event ~p~n", [[shutdown_user]]),
    {keep_state, Data, [postpone]};
s1(EventType, {APIPid, {ready}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {APIPid, {ready}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

