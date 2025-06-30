-module(gen_b).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 s3/3
	 ]).

-include("b.hrl").
-type state_data() :: #state_data{a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s3(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {next_state, s3, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("b: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s3(EventType, {CPid, {pong}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {CPid, {pong}}, Data).

-spec s1(term(), {pid(), {atom(), term()}}, state_data()) -> term().
s1(EventType, Event, Data) ->
    Callback = get(callback_module),
    Callback:s1(EventType, Event, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.
