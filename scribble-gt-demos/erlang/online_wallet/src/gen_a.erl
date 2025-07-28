-module(gen_a).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 send_s3_login_success/2, 
	 s3/3, 
	 send_s3_login_failed/2, 
	 send_s4_login_accepted/2, 
	 s4/3, 
	 s8/3, 
	 send_s12_auth_fail/2, 
	 s12/3
	 ]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), c_pid :: pid() | undefined, s_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {login_accepted}}]} | {keep_state, state_data()} | {next_state, s12, state_data(), [{next_event, internal, {auth_fail}}]}.
-callback s4(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s8(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s8, state_data()} | {stop, normal, state_data()} | {keep_state, state_data(), [postpone]}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s3, state_data(), [{next_event, internal, {login_success}}]} | {next_state, s3, state_data(), [{next_event, internal, {login_failed}}]} | {keep_state, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_a, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "a_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("a: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s12_auth_fail(SPid :: pid(), _Data :: state_data()) -> ok.
send_s12_auth_fail(SPid, _Data) ->
    gen_statem:cast(SPid, {self(), {auth_fail}}).

-spec s3(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s4, state_data(), [{next_event, internal, {login_accepted}}]} |
    {keep_state, state_data()} |
    {next_state, s12, state_data(), [{next_event, internal, {auth_fail}}]}.
s3(EventType, {login_success}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {login_success}, Data);
s3(EventType, {login_failed}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {login_failed}, Data).

-spec s4(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data()}.
s4(EventType, {login_accepted}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {login_accepted}, Data).

-spec s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(EventType, {auth_fail}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {auth_fail}, Data).

-spec s8(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s8, state_data()} |
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]}.
s8(_EventType, {_Pid, {keep_alive}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[keep_alive]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {end_session}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_a: Postponing event ~p~n", [[end_session]]),
    {keep_state, Data, [postpone]};
s8(EventType, {CPid, {keep_alive}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {CPid, {keep_alive}}, NewData);
s8(EventType, {CPid, {end_session}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {CPid, {end_session}}, NewData);
s8(EventType, {SPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {SPid, {timeout}}, NewData);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {timeout} 
		orelse Msg =:= {keep_alive} 
		orelse Msg =:= {end_session} ->
    io:format("gen_a: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s3_login_success(CPid :: pid(), _Data :: state_data()) -> ok.
send_s3_login_success(CPid, _Data) ->
    gen_statem:cast(CPid, {self(), {login_success}}).

-spec send_s4_login_accepted(SPid :: pid(), _Data :: state_data()) -> ok.
send_s4_login_accepted(SPid, _Data) ->
    gen_statem:cast(SPid, {self(), {login_accepted}}).

-spec send_s3_login_failed(CPid :: pid(), _Data :: state_data()) -> ok.
send_s3_login_failed(CPid, _Data) ->
    gen_statem:cast(CPid, {self(), {login_failed}}).

-spec s1(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s3, state_data(), [{next_event, internal, {login_success}}]} |
    {next_state, s3, state_data(), [{next_event, internal, {login_failed}}]} |
    {keep_state, state_data()}.
s1(_EventType, {_Pid, {keep_alive}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[keep_alive]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[timeout]]),
    {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {end_session}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_a: Postponing event ~p~n", [[end_session]]),
    {keep_state, Data, [postpone]};
s1(EventType, {CPid, {login, Id, Password}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {CPid, {login, Id, Password}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

