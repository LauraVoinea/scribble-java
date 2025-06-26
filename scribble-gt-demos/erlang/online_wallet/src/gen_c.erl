-module(gen_c).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s1_login/4, s1/3, s3/3, s5/3, send_s8_quit/2, s8/3, send_s8_pay/4, s9/3, send_s10_keep_alive/2, s10/3, s13/3, send_s14_end_session/2, s14/3]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, s_pid :: pid() | undefined}.

-callback s3(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
-callback s5(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s8, state_data()} | {next_state, s8, state_data(), [term()]}.
-callback s10(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
-callback s13(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data(), [{next_event, internal, {end_session}}]} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s8(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s13, state_data()} | {next_state, s9, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s9(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s10, state_data(), [{next_event, internal, {keep_alive}}]} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s14(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {login}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args}, 
            % []);
            [{debug, [trace, {log_to_file, "c_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {login}}]}.
init({CallbackModule, _Args}) ->
    io:format("c: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(term(), {pid(), {atom()}}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
s3(EventType, {APid, {login_success}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {APid, {login_success}}, Data);
s3(EventType, {APid, {login_failed}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {APid, {login_failed}}, Data).

-spec s5(term(), {pid(), {atom(), term(), term()}}, state_data()) -> {next_state, s8, state_data()} | {next_state, s8, state_data(), [term()]}.
s5(EventType, {SPid, {account, Balance, Overdraft}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {SPid, {account, Balance, Overdraft}}, Data).

-spec s10(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data()}.
s10(EventType, {keep_alive}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {keep_alive}, Data).

-spec s13(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data(), [{next_event, internal, {end_session}}]} | {stop, normal, state_data()} | {keep_state, state_data()}.
s13(EventType, {SPid, {quit_ack}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {SPid, {quit_ack}}, Data);
s13(EventType, {SPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {SPid, {timeout}}, Data);
s13(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {timeout} 
		orelse Msg =:= {login_failed} 
		orelse Msg =:= {login_success} 
		orelse Msg =:= {quit_ack} 
		orelse Msg =:= {confirmation} ->
    {keep_state, Data};
s13(_EventType, {_Pid, {account, _Balance, _Overdraft}, _Counter}, Data) ->
    {keep_state, Data}.

-spec s8(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s13, state_data()} | {next_state, s9, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s8(EventType, {quit}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {quit}, NewData);
s8(EventType, {pay}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {pay}, NewData);
s8(EventType, {SPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {SPid, {timeout}}, Data);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {timeout} 
		orelse Msg =:= {login_failed} 
		orelse Msg =:= {login_success} 
		orelse Msg =:= {quit_ack} 
		orelse Msg =:= {confirmation} ->
    {keep_state, Data};
s8(_EventType, {_Pid, {account, _Balance, _Overdraft}, _Counter}, Data) ->
    {keep_state, Data}.

-spec s9(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s10, state_data(), [{next_event, internal, {keep_alive}}]} | {stop, normal, state_data()} | {keep_state, state_data()}.
s9(EventType, {SPid, {confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {SPid, {confirmation}}, Data);
s9(EventType, {SPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {SPid, {timeout}}, Data);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {timeout} 
		orelse Msg =:= {login_failed} 
		orelse Msg =:= {login_success} 
		orelse Msg =:= {quit_ack} 
		orelse Msg =:= {confirmation} ->
    {keep_state, Data};
s9(_EventType, {_Pid, {account, _Balance, _Overdraft}, _Counter}, Data) ->
    {keep_state, Data}.

-spec s14(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s14(EventType, {end_session}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {end_session}, Data).

-spec send_s8_pay(SPid :: pid(), Payee :: term(), Amount :: term(), Data :: state_data()) -> ok.
send_s8_pay(SPid, Payee, Amount, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SPid, {self(), {pay, Payee, Amount}, Counter}).

-spec send_s1_login(APid :: pid(), Id :: term(), Password :: term(), _Data :: state_data()) -> ok.
send_s1_login(APid, Id, Password, _Data) ->
    gen_statem:cast(APid, {self(), {login, Id, Password}}).

-spec send_s14_end_session(APid :: pid(), Data :: state_data()) -> ok.
send_s14_end_session(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {end_session}, Counter}).

-spec s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data()}.
s1(EventType, {login}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {login}, Data).

-spec send_s10_keep_alive(APid :: pid(), Data :: state_data()) -> ok.
send_s10_keep_alive(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {keep_alive}, Counter}).

-spec send_s8_quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s8_quit(SPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SPid, {self(), {quit}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

