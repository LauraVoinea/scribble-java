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
-record(state_data, {bob_pid}).

% Function to start the state machine
start_link(BobPid) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [BobPid], []).

callback_mode() -> [state_functions].

init([BobPid]) ->
  io:format("Alice: Init~n"),
  BobPid ! {alice_pid, self()},
  initiate_ping_loop(),
  {ok, state1, #state_data{bob_pid = BobPid}}.

% Function to start the ping loop
initiate_ping_loop() ->
  gen_statem:cast(self(), {ping_start}).

% State1: Wait for the first ping initiation
state1(cast, {ping_start}, Data) ->
  io:format("Alice: State1: Initiating ping loop~n"),
  send_ping(self()),
  {next_state, state2, Data}.
%%state1(cast, {ping}, Data) ->
%%  io:format("Alice: State1: Postponing ping event~n"),
%%  {keep_state_and_data, [{postpone}]}.

% State2: Send ping to Bob
state2(cast, {error}, Data) ->
  io:format("Alice: State2: Received error. Goodbye!~n"),
  {stop, normal, Data};
state2(cast, {ping}, #state_data{bob_pid = BobPid} = Data) ->
  io:format("Alice: State2: Sending ping to Bob~n"),
  send_ping(BobPid),
  {next_state, state3, Data}.
%%state2(_, _, Data) ->
%%  io:format("Alice: State2: Postponing event~n"),
%%  {keep_state, Data, [postpone]}.

% State3: Wait for pong or handle error
state3(cast, {error}, Data) ->
  io:format("Alice: State3: Received error. Goodbye!~n"),
  {stop, normal, Data};
state3(cast, {pong}, Data) ->
  io:format("Alice: State3: Received pong from Bob~n"),
  initiate_ping_loop(),
  {next_state, state1, Data}.
%%state3(_, _, Data) ->
%%  io:format("Alice: State3: Postponing event~n"),
%%  {keep_state_and_data, Data, [postpone]}.

% Send ping function
send_ping(BobPid) ->
  gen_statem:cast(BobPid, {ping}).
