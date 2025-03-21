-module(gen_b).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_Timeout/2, s1/3, send_a2/2, s2/3, send_a5/2, s3/3]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()}.
-callback s1(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s3, state_data()} | {next_state, s3, state_data(), [term()]} | {next_state, s2, state_data(), [{next_event, internal, {a2}}]} | {keep_state, state_data()}.
-callback s2(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data()} | {next_state, s3, state_data(), [term()]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> gen_statem:callback_mode().
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data(), [{next_event, internal, {Timeout}}]}.
init({CallbackModule, _Args}) ->
    io:format("b: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()}.
s3(EventType, {a5}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {a5}, Data);
s3(EventType, {'Timeout'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {'Timeout'}, Data).

-spec send_Timeout(APid :: pid() | CPid :: pid(), Data :: state_data()) -> ok | ok.
send_Timeout(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {'Timeout'}, Counter});
send_Timeout(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'Timeout'}, Counter}).

-spec send_a5(APid :: pid(), Data :: state_data()) -> ok.
send_a5(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {a5}, Counter}).

-spec send_a2(CPid :: pid(), Data :: state_data()) -> ok.
send_a2(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a2}, Counter}).

-spec s1(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s3, state_data()} | {next_state, s3, state_data(), [term()]} | {next_state, s2, state_data(), [{next_event, internal, {a2}}]} | {keep_state, state_data()}.
s1(EventType, {'Timeout'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {'Timeout'}, Data);
s1(EventType, {APid, {a1}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter == MC ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {APid, {a1}}, NewData);
s1(_EventType, {_Pid, _Label, _Counter}, Data) ->
    {keep_state, Data}.

-spec s2(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data()} | {next_state, s3, state_data(), [term()]}.
s2(EventType, {a2}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s2(EventType, {a2}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

