-module(b).
-behaviour(gen_b).

-export([init/1, callback_mode/0, start_link/0, make_choice_To/1, s5/3, make_choice_a1/1, s6/3, s7/3, s3/3]).

-include("b.hrl").

%% @doc
%% This module implements the callback functions for the `gen_b` behaviour,
%% defining the application-specific logic for role 'B' in the Timeout protocol.
%% It manages interactions with roles 'A' and 'C' through a state machine.
%%
%% The `gen_b` behaviour (wrapper around `gen_statem`) handles the
%% underlying state machine mechanics, while this module provides the custom
%% logic for state transitions and message handling based on the protocol defined
%% in `Timeout.scr`.
%%
%% The state is maintained in the `state_data` record, defined in `b.hrl`,
%% which includes PIDs for roles 'A' and 'C' and a message counter.
%%
%% States:
%% - `s5`: Initial state after `init`. Handles internal 'To' events and external 'a1' messages from A.
%%          Makes choices to transition to `s3` or `s6`.
%% - `s3`: Sends 'To' to C and terminates.
%% - `s6`: Sends 'a2' to C and transitions to `s7`.
%% - `s7`: Sends 'a5' to A and terminates.
%% @end

%% @doc Starts the gen_statem process for role B.
%% Links the process to the caller. Uses `gen_b:start_link` which
%% wraps `gen_statem:start_link` and passes this module (`?MODULE`) as the
%% callback module.
%% @returns {ok, pid()} | {error, term()}.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

%% @doc Specifies the callback mode for `gen_statem`.
%% @returns `state_functions`.
-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

%% @doc Initializes the state machine for role B.
%% Waits until processes registered as 'a' and 'c' are available, retrying
%% after a delay if necessary. Stores their PIDs in the `state_data` record.
%% Initializes the message counter `mc_counter_1` to 0.
%% Emits an internal `{'To'}` event to trigger the first action and move to
%% state `s5`s.
%% @returns {ok, initial_state_name, initial_state_data, actions}.
-spec init(list()) -> {ok, s5, state_data(), [{next_event, internal, {'To'}}]}.
init([]) ->
    APid = case whereis(a) of
        undefined ->
            io:format("B: a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid ->
            Pid
    end,
    CPid = case whereis(client) of
        undefined ->
            io:format("B: c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(client);
        Pid1 ->
            Pid1
    end,
    Data = #state_data{mc_counter_1 = 0, a_pid = APid, c_pid = CPid},
    io:format("B: b initialized ~n", []),
    {ok, s5, Data, [{next_event, internal, {'To'}}]}.

%% @doc State handler for state `s5` -- Mixed Choice State.
%% Handles two types of events:
%% 1. `internal, {'To'}`:
%%    - Calls `make_choice_To/1` to decide the next action randomly (1 or 2).
%%    - If choice is 1: Stays in state `s5` and waits for the message from A (`keep_state`).
%%    - If choice is 2: Sends a `'To'` message to A using `gen_b:send_s5_To/2`
%%      and transitions to state `s3`, posting an internal `{'To'}` event.
%% 2. `cast, {APid, {a1}}`:
%%    - Handles an `a1` message received from process A (`APid`).
%%    - Calls `make_choice_a1/1` to decide the next action randomly (1 or 2).
%%    - If choice is 1: Transitions to state `s6`, posting an internal `{a2}` event.
%%    - If choice is 2: Sends a `'To'` message back to A using `gen_b:send_s5_To/2`
%%      and transitions to state `s3`, emitting an internal `{'To'}` event.
%% @param EventType The type of event (`internal` or `cast`).
%% @param EventContent The event content (`{'To'}` or `{Pid, {a1}}`).
%% @param Data The current state data.
%% @returns `{next_state, ...}`, `{keep_state, ...}`.
-spec s5(internal | cast, {'To'} | {pid(), {a1}}, state_data()) ->
    {next_state, s3, state_data(), [{next_event, internal, {'To'}}]} |
    {next_state, s6, state_data(), [{next_event, internal, {a2}}]}.
s5(internal, {'To'}, Data) ->
    APid = case Data#state_data.a_pid of
                   undefined -> receive {a_pid, Pid} -> Pid end;
                   Pid -> Pid
               end,
    CPid = case Data#state_data.c_pid of
               undefined -> receive {c_pid, Pid1} -> Pid1 end;
               Pid1 -> Pid1
           end,
    NewData = Data#state_data{a_pid = APid, c_pid = CPid},

    case make_choice_To(NewData) of
        1 ->
            {keep_state, NewData};
        2 ->
            io:format("B: s5 Sending To to A ~p ~n", [Data]),
            gen_b:send_s5_To(APid, Data),
            {next_state, s3, NewData, [{next_event, internal, {'To'}}]}
    end;
s5(cast, {APid, {a1}}, #state_data{a_pid = APid} = Data) ->
    case make_choice_a1(Data) of
        1 ->
            io:format("B: s5 received a1 ~p ~n", [Data]),
            {next_state, s6, Data, [{next_event, internal, {a2}}]};
        2 ->
            io:format("B: s5 received a1, going to state3 To ~p ~n", [Data]),
            gen_b:send_s5_To(APid, Data),
            {next_state, s3, Data, [{next_event, internal, {'To'}}]}
    end.

%% @doc State handler for state `s3`.
%% Handles the internal `{'To'}` event. Sends a `'To'` message to process C
%% using `gen_b:send_s3_To/2` and then stops the state machine normally.
%% @param _EventType The type of event (internal).
%% @param _EventContent The event content (`{'To'}`).
%% @param Data The current state data, containing the PID for C.
%% @returns `{stop, normal, updated_state_data}`.
-spec s3(internal, {'To'}, state_data()) -> {stop, normal, state_data()}.
s3(internal, {'To'}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s3 Sending To to C ~n", []),
    gen_b:send_s3_To(CPid, Data),
    {stop, normal, Data}.

%% @doc State handler for state `s6`.
%% Handles the internal `{a2}` event. Sends an `a2` message to process C
%% using `gen_b:send_s6_a2/2`. Transitions to state `s7`, emitting an
%% internal `{a5}` event.
%% @param _EventType The type of event (internal).
%% @param _EventContent The event content (`{a2}`).
%% @param Data The current state data, containing the PID for C.
%% @returns `{next_state, s7, updated_state_data, actions}`.
-spec s6(internal, {a2}, state_data()) ->
    {next_state, s7, state_data(), [{next_event, internal, {a5}}]}.
s6(internal, {a2}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s6 sending a2 ~n", []),
    gen_b:send_s6_a2(CPid, Data),
    {next_state, s7, Data, [{next_event, internal, {a5}}]}.

%% @doc State handler for state `s7`.
%% Handles the internal `{a5}` event. Sends an `a5` message to process A
%% using `gen_b:send_s7_a5/2` and then stops the state machine normally.
%% @param _EventType The type of event (internal).
%% @param _EventContent The event content (`{a5}`).
%% @param Data The current state data, containing the PID for A.
%% @returns `{stop, normal, updated_state_data}`.
-spec s7(internal, {a5}, state_data()) -> {stop, normal, state_data()}.
s7(internal, {a5}, #state_data{a_pid = APid} = Data) ->
    io:format("B: sending a5 ~n", []),
    gen_b:send_s7_a5(APid, Data),
    {stop, normal, Data}.

%% @doc Makes a random choice (1 or 2) for the 'To' event handling in state `s5`.
%% @param _Data The current state data.
%% @returns An integer, 1 or 2.
-spec make_choice_To(state_data()) -> integer().
make_choice_To(_Data) ->
    rand:uniform(2).

%% @doc Makes a random choice (1 or 2) for the 'a1' message handling in state `s5`.
%% @param _Data The current state data.
%% @returns An integer, 1 or 2.
-spec make_choice_a1(state_data()) -> integer().
make_choice_a1(_Data) ->
    rand:uniform(2).
