-module(gen_c).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s1_pong/2, 
	 s1/3
	 ]).

-include("c.hrl").
-type state_data() :: #state_data{a_pid :: pid() | undefined, b_pid :: pid() | undefined}.

-callback s1(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {pong}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args},
%%              []);
              [{debug, [trace, {log_to_file, "c_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {pong}}]}.
init({CallbackModule, _Args}) ->
    io:format("c: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s1_pong(BPid :: pid(), _Data :: state_data()) -> ok.
send_s1_pong(BPid, Data) ->
    gen_statem:cast(BPid, {self(), {pong}}).

-spec s1(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s1(EventType, {pong}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {pong}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

