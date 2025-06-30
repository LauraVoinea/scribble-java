-module(gen_a).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s1_ping/2, 
	 s1/3
	 ]).

-include("a.hrl").
-type state_data() :: #state_data{b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-callback s1(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {ping}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_a, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {ping}}]}.
init({CallbackModule, _Args}) ->
    io:format("a: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s1_ping(BPid :: pid(), _Data :: state_data()) -> ok.
send_s1_ping(BPid, Data) ->
    gen_statem:cast(BPid, {self(), {ping}}).

-spec s1(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s1(EventType, {ping}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {ping}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

