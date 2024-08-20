-module(bob).
% Define a macro SERVER that is replaced with ?MODULE (current module name)
-define(SERVER, ?MODULE).
-behaviour(gen_statem).
-compile(export_all).
-compile(nowarn_export_all).

% Function to start the state machine
start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions].

init([]) -> {ok, state1, {}}.

send_pong(AlicePid) ->
  io:format("Bob: Sending pong to Alice~n"),
  gen_statem:cast(AlicePid, {pong}).

state1(cast, ping, Data) ->
  io:format("Bob: Received ping from Alice~n"),
  io:format("Bob: Switching to State2~n"),
  {next_state, state2, Data, [{next_event, internal, {pong}}]}.

state2(internal, {pong}, Data) ->
  io:format("Bob: Sending pong to Alice from State2~n"),
  send_pong(whereis(alice)),
  io:format("Bob: Good bye!~n"),
  {stop, normal, Data}.

