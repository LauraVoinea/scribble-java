-module(gen_a).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_a1/2, s1/3, s2/3, send_a6/2, s3/3]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s1(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s2, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s2(EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s3, state_data(), [{next_event, internal, {a6}}]} | {keep_state, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_a, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> gen_statem:callback_mode().
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data(), [{next_event, internal, {a1}}]}.
init({CallbackModule, _Args}) ->
    io:format("a: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(EventType, {a6}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {a6}, Data).

-spec send_a1(BPid :: pid(), Data :: state_data()) -> ok.
send_a1(BPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BPid, {self(), {a1}, Counter}).

-spec s1(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s2, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s1(EventType, {a1}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {a1}, NewData);
s1(EventType, {BPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter == MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {BPid, {'Timeout'}}, Data);
s1(_EventType, {_Pid, _Label, _Counter}, Data) ->
    {keep_state, Data}.

-spec send_a6(CPid :: pid(), Data :: state_data()) -> ok.
send_a6(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a6}, Counter}).

-spec s2(EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {next_state, s3, state_data(), [{next_event, internal, {a6}}]} | {keep_state, state_data()}.
s2(EventType, {BPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter == MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s2(EventType, {BPid, {'Timeout'}}, Data);
s2(EventType, {BPid, {a5}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter == MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s2(EventType, {BPid, {a5}}, Data);
s2(_EventType, {_Pid, _Label, _Counter}, Data) ->
    {keep_state, Data}.

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

