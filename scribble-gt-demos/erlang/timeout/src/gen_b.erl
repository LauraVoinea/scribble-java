-module(gen_b).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s5_TOa/2, 
	 s5/3, 
	 send_s6_a3/2, 
	 s6/3, 
	 send_s7_a4/2, 
	 s7/3, 
	 send_s3_TOc/2, 
	 s3/3
	 ]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s5, state_data(), [{next_event, internal, {'TOa'}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args},
              [{debug, [trace, {log_to_file, "b_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s5, state_data(), [{next_event, internal, {'TOa'}}]}.
init({CallbackModule, _Args}) ->
    io:format("b: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(EventType, {'TOc'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {'TOc'}, Data).

s5(EventType, {'TOa'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {'TOa'}, NewData);
s5(EventType, {APid, {a1}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {APid, {a1}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {a1} ->
    io:format("gen_b: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

s6(EventType, {a3}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {a3}, Data).

-spec s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(EventType, {a4}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {a4}, Data).

-spec send_s5_TOa(APid :: pid(), Data :: state_data()) -> ok.
send_s5_TOa(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {'TOa'}, Counter}).

-spec send_s3_TOc(CPid :: pid(), Data :: state_data()) -> ok.
send_s3_TOc(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'TOc'}, Counter}).

-spec send_s7_a4(APid :: pid(), Data :: state_data()) -> ok.
send_s7_a4(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {a4}, Counter}).

-spec send_s6_a3(CPid :: pid(), Data :: state_data()) -> ok.
send_s6_a3(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a3}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

