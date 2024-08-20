-module(alice).
% Define a macro SERVER that is replaced with ?MODULE (current module name)
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
  {ok, state2, #state_data{bob_pid = BobPid}, [{timeout, 100, ping}]}. % 100-millisecond delay

send_ping(BobPid) ->
  gen_statem:cast(BobPid, {ping}).

% Mixed choice
state2(cast, {error}, Data) ->
  io:format("Alice: State2: Received Error from Bob~n"),
  io:format("Alice: Good bye!~n"),
  {stop, normal, Data};
state2(timeout, ping, #state_data{bob_pid = BobPid} = Data) ->
  io:format("Alice: State2: Sending ping to Bob~n"),
  send_ping(BobPid),
  {next_state, state3, Data}.

state3(cast, {error}, Data) ->
  io:format("Alice: State3: Received Error from Bob~n"),
  io:format("Alice: Good bye!~n"),
% Stop the state machine with normal termination
  {stop, normal, Data};
state3(cast, {pong}, #state_data{bob_pid = BobPid} = Data) ->
  io:format("Alice: State3: Received pong from Bob~n"),
  {next_state, state2, Data, [{timeout, 100, ping}]}.  %100-millisecond delay
