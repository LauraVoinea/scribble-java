-module(gen_s).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, s1/3, send_s4_account/4, s4/3, send_s8_timeout/2, s8/3, send_s9_confirmation/2, s9/3, send_s12_quit_ack/2, s12/3, send_s6_timeout/2, s6/3]).

-include("s.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data(), [{next_event, internal, {timeout}}]}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s8(EventType :: term(), {atom()} | {pid() | undefined, {term()}, integer()} | term(), state_data()) -> {next_state, s6, state_data(), [{next_event, internal, {timeout}}]} | {next_state, s9, state_data(), [{next_event, internal, {confirmation}}]} | {next_state, s12, state_data(), [{next_event, internal, {quit_ack}}]} | {keep_state, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {account}}]}.
-callback s1(term(), {pid() | undefined, {atom(), term()}}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {account}}]} | {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_s, {CallbackModule, Args}, 
            % []);
            [{debug, [trace, {log_to_file, "s_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("s: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s9_confirmation(CPid :: pid() | undefined, Data :: state_data()) -> ok.
send_s9_confirmation(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {confirmation}, Counter}).

-spec send_s6_timeout(APid :: pid() | undefined, Data :: state_data()) -> ok.
send_s6_timeout(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {timeout}, Counter}).

-spec s4(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data(), [{next_event, internal, {timeout}}]}.
s4(EventType, {account}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {account}, Data).

-spec s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(EventType, {timeout}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {timeout}, Data).

-spec send_s12_quit_ack(CPid :: pid() | undefined, Data :: state_data()) -> ok.
send_s12_quit_ack(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {quit_ack}, Counter}).

-spec s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(EventType, {quit_ack}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {quit_ack}, Data).

-spec s8(EventType :: term(), {atom()} | {pid() | undefined, {term()}, integer()} | term(), state_data()) -> {next_state, s6, state_data(), [{next_event, internal, {timeout}}]} | {next_state, s9, state_data(), [{next_event, internal, {confirmation}}]} | {next_state, s12, state_data(), [{next_event, internal, {quit_ack}}]} | {keep_state, state_data()}.
s8(EventType, {timeout}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {timeout}, NewData);
s8(EventType, {CPid, {pay, Payee, Amount}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {CPid, {pay, Payee, Amount}}, Data);
s8(EventType, {CPid, {quit}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {CPid, {quit}}, Data);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {auth_fail} 
		orelse Msg =:= {quit} 
		orelse Msg =:= {login_accepted} ->
    {keep_state, Data};
s8(_EventType, {_Pid, {pay, _Payee, _Amount}, _Counter}, Data) ->
    {keep_state, Data}.

-spec s9(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {account}}]}.
s9(EventType, {confirmation}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {confirmation}, Data).

-spec send_s8_timeout(CPid :: pid() | port() | undefined, Data :: state_data()) -> ok.
send_s8_timeout(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {timeout}, Counter}).

-spec send_s4_account(CPid :: pid() | undefined, Balance :: term(), Overdraft :: term(), _Data :: state_data()) -> ok.
send_s4_account(CPid, Balance, Overdraft, _Data) ->
    gen_statem:cast(CPid, {self(), {account, Balance, Overdraft}}).

-spec s1(term(), {pid() | undefined, {atom(), term()}}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {account}}]} | {stop, normal, state_data()}.
s1(EventType, {APid, {login_accepted}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {APid, {login_accepted}}, Data);
s1(EventType, {APid, {auth_fail}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {APid, {auth_fail}}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

