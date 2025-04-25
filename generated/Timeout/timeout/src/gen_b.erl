-module(gen_b).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s5_To/2, s5/3, send_s6_a2/2, s6/3, send_s7_a5/2, s7/3, send_s3_To/2, s3/3]).

-include("b.hrl").

%% @doc
%% Generic `gen_statem` behaviour implementation for role 'B' in a distributed protocol.
%% This module defines the structure of the state machine (states, transitions) and
%% handles the generic mechanics of the protocol, such as message passing,
%% state transitions, and basic event handling including mixed-choice counter logic.
%%
%% It relies on a specific *callback module* (e.g., `b.erl`) passed during `start_link/2`
%% to implement the actual application-specific logic for each state and event.
%% This module forwards events to the corresponding callback functions.
%%
%% It also provides helper functions (`send_*`) for the callback module to send
%% messages associated with specific states. Message sending includes a counter
%% (`mc_counter_1` from `state_data`) to handle potential mixed-choice scenarios
%% and ensure message ordering or relevance.
%%
%% This module is typically *not* edited by the user implementing the protocol logic;
%% instead, the user implements the required callbacks in a separate module.
%% @end

%% @doc Callback specification for the initialization function.
%% The callback module must implement `init/1`.
-callback init(Args :: list()) -> {ok, s5, state_data(), [{next_event, internal, {'To'}}]}.
%% @doc Callback specification for the state `s3` handler.
%% The callback module must implement `s3/3`.
-callback s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
%% @doc Callback specification for the state `s5` handler.
%% The callback module must implement `s5/3`. This state handles internal events
%% and external messages.
-callback s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) ->
    {next_state, s3, state_data(), [{next_event, internal, {'To'}}]} |
    {next_state, s6, state_data(), [{next_event, internal, {a2}}]} |
    {keep_state, state_data()}.

%% @doc Callback specification for the state `s6` handler.
%% The callback module must implement `s6/3`.
-callback s6(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s7, state_data(), [{next_event, internal, {a5}}]}.

%% @doc Callback specification for the state `s7` handler.
%% The callback module must implement `s7/3`.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.

%% @doc Starts the `gen_statem` process for role B.
%% Ensures the provided `CallbackModule` is loaded and then starts the
%% state machine using `gen_statem:start_link/4`. The process is registered
%% locally with the name of the `CallbackModule`. Debug tracing is enabled,
%% logging to "b_debug.log".
%% @param CallbackModule The module implementing the application-specific logic (callbacks).
%% @param Args Arguments to pass to the `CallbackModule:init/1` function.
%% @returns {ok, pid()} | {error, term()}.
-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_b, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "b_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Specifies the callback mode for `gen_statem`.
%% `state_functions` mode means each state is handled by a function with the same name.
%% @returns `state_functions`.
-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

%% @doc Sends the `a5` message (from state `s7`) to process A.
%% Uses `gen_statem:cast` to send the message asynchronously.
%% Includes the current mixed-choice counter (`mc_counter_1`) from the state data.
%% @param APid The PID of the target process (role A).
%% @param Data The current state data containing the counter.
%% @returns `ok`.
-spec send_s7_a5(APid :: pid(), Data :: state_data()) -> ok.
send_s7_a5(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {a5}, Counter}).

%% @doc Sends the `a2` message (from state `s6`) to process C.
%% Uses `gen_statem:cast` to send the message asynchronously.
%% Includes the current mixed-choice counter (`mc_counter_1`) from the state data.
%% @param CPid The PID of the target process (role C).
%% @param Data The current state data containing the counter.
%% @returns `ok`.
-spec send_s6_a2(CPid :: pid(), Data :: state_data()) -> ok.
send_s6_a2(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {a2}, Counter}).

%% @doc Sends the `To` message (from state `s5`) to process A.
%% Uses `gen_statem:cast` to send the message asynchronously.
%% Includes the current mixed-choice counter (`mc_counter_1`) from the state data.
%% @param APid The PID of the target process (role A).
%% @param Data The current state data containing the counter.
%% @returns `ok`.
-spec send_s5_To(APid :: pid(), Data :: state_data()) -> ok.
send_s5_To(APid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(APid, {self(), {'To'}, Counter}).

%% @doc Sends the `To` message (from state `s3`) to process C.
%% Uses `gen_statem:cast` to send the message asynchronously.
%% Includes the current mixed-choice counter (`mc_counter_1`) from the state data.
%% @param CPid The PID of the target process (role C).
%% @param Data The current state data containing the counter.
%% @returns `ok`.
-spec send_s3_To(CPid :: pid(), Data :: state_data()) -> ok.
send_s3_To(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'To'}, Counter}).

%% @doc `gen_statem` initialization callback.
%% Stores the `CallbackModule` name in the process dictionary and then calls
%% the `init/1` function of the `CallbackModule` to get the initial state,
%% data, and actions.
%% @param InitArgs A tuple `{CallbackModule, Args}` passed from `start_link/2`.
%% @returns The result of `CallbackModule:init/1`.
-spec init({atom(), [any()]}) -> {ok, s5, state_data(), [{next_event, internal, {'To'}}]}.
init({CallbackModule, _Args}) ->
    io:format("B: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

%% @doc State handler function for state `s3`.
%% Retrieves the callback module name from the process dictionary and delegates
%% the event handling to `CallbackModule:s3/3`.
%% @param EventType The type of event.
%% @param EventContent The event content (expected `{'To'}`).
%% @param Data The current state data.
%% @returns The result of `CallbackModule:s3/3`.
-spec s3(EventType :: term(), {'To'}, state_data()) -> {stop, normal, state_data()}.
s3(EventType, {'To'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {'To'}, Data).

%% @doc State handler function for state `s5`.
%% Handles internal `{'To'}` events and external `a1` messages.
%% Retrieves the callback module name from the process dictionary.
%% - For internal `{'To'}`: Increments the `mc_counter_1` and delegates to `CallbackModule:s5/3`.
%% - For external `{APid, {a1}, Counter}`: Checks if the incoming `Counter` matches the
%%   current `mc_counter_1`. If they match, delegates to `CallbackModule:s5/3` with
%%   the simplified event `{APid, {a1}}`.
%%   Discards stale message by returning `{keep_state, Data}` (message purging).
%% @param EventType The type of event (`internal` or `cast`).
%% @param EventContent The event content.
%% @param Data The current state data.
%% @returns The result of the corresponding `CallbackModule:s5/3` call or `{keep_state, Data}`.
-spec s5(term(), {atom()} | {pid(), {term()}, integer()} | term(), state_data()) ->
    {next_state, s3, state_data(), [{next_event, internal, {'To'}}]} |
    {next_state, s6, state_data(), [{next_event, internal, {a2}}]} |
    {keep_state, state_data()}.
s5(EventType, {'To'},  #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
%%    io:format("B: s5 TO MC NewData ~p ~p~n", [MC, NewData]),
    CallbackModule:s5(EventType, {'To'}, NewData);
s5(EventType, {APid, {a1}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
%%    io:format("B: s5 APid ~p, Counter ~p, MC ~p~n", [APid, Counter, MC]),
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {APid, {a1}}, Data);
s5(_EventType, {_Pid, {Msg}, _Counter}, Data) when Msg =:= a6
		orelse Msg =:= 'To'
		orelse Msg =:= a1 ->
    {keep_state, Data}.


%% @doc State handler function for state `s6`.
%% Retrieves the callback module name from the process dictionary and delegates
%% the event handling for `{a2}` to `CallbackModule:s6/3`.
%% @param EventType The type of event.
%% @param EventContent The event content (expected `{a2}`).
%% @param Data The current state data.
%% @returns The result of `CallbackModule:s6/3`.
-spec s6(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s7, state_data(), [{next_event, internal, {a5}}]}.
s6(EventType, {a2}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {a2}, Data).

%% @doc State handler function for state `s7`.
%% Retrieves the callback module name from the process dictionary and delegates
%% the event handling for `{a5}` to `CallbackModule:s7/3`.
%% @param EventType The type of event.
%% @param EventContent The event content (expected `{a5}`).
%% @param Data The current state data.
%% @returns The result of `CallbackModule:s7/3`.
-spec s7(EventType :: term(), {atom()}, state_data()) ->
    {stop, normal, state_data()}.
s7(EventType, {a5}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {a5}, Data).

%% @doc `gen_statem` code change callback.
%% Handles code upgrades. The default implementation simply returns the current state.
%% @param _OldVsn The old version.
%% @param _StateName The current state name.
%% @param StateData The current state data.
%% @param _Extra Extra data.
%% @returns `{ok, StateData}`.
-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

%% @doc `gen_statem` termination callback.
%% Called when the state machine is about to terminate.
%% The default implementation does nothing.
%% @param _Reason The reason for termination.
%% @param _State The current state name.
%% @param _StateData The current state data.
%% @returns `ok`.
-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

