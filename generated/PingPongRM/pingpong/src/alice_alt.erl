%%%-------------------------------------------------------------------
%%% @author crypt
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Aug 2024 15:54
%%%-------------------------------------------------------------------
-module(alice_alt).
-author("crypt").
-define(SERVER, ?MODULE).
-behaviour(gen_statem).
-compile(export_all).
-compile(nowarn_export_all).
-record(state_data, {bob_pid, recursion_counter = 0}).

% Function to start the state machine
start_link(BobPid) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [BobPid], []).

callback_mode() -> [state_functions].

init([BobPid]) ->
  io:format("Alice: Init~p~p~n", [BobPid, self()]),

  BobPid ! {alice_pid, self()},
  initiate_ping_loop(),
  {ok, state1, #state_data{bob_pid = BobPid, recursion_counter = 0}}.

% Function to start the ping loop
initiate_ping_loop() ->
  gen_statem:cast(self(), {ping_start}).

% State1: Wait for the first ping initiation
state1(cast, {ping_start}, #state_data{recursion_counter = Counter} = Data) ->
  io:format("Alice: State1: Initiating ping loop~p~n", [Counter]),
  NewCounter = Counter + 1,
  send_ping(self(), NewCounter),
  {next_state, state2, Data#state_data{recursion_counter = NewCounter}}.

% State2: Send ping to Bob
state2(cast, {error, BobPid}, Data) ->
  io:format("Alice: State2: Received error. Goodbye!~n"),
  {stop, normal, Data};
state2(cast, {ping, Counter, AlicePid}, #state_data{bob_pid = BobPid, recursion_counter = Counter} = Data) ->
%%  NewCounter = Counter + 1,
  io:format("Alice: State2: Sending ping to Bob~n"),
  send_ping(BobPid, Counter),
  {next_state, state3, Data#state_data{recursion_counter = Counter}}.

% State3: Wait for pong or handle error
state3(cast, {error, Counter, BobPid}, Data) ->
  io:format("Alice: State3: Received error. Goodbye!~n"),
  {stop, normal, Data};
state3(cast, {pong, Counter, BobPid}, #state_data{recursion_counter = Counter} = Data) ->
  io:format("Alice: State3: Received pong from Bob~n"),
%%  NewCounter = Counter + 1,
  initiate_ping_loop(),
  {next_state, state1, Data#state_data{recursion_counter = Counter}}.

% Send ping function
send_ping(BobPid, Counter) ->
  gen_statem:cast(BobPid, {ping, Counter, self()}).
