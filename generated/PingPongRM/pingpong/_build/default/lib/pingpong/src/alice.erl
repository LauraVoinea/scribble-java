-module(alice).
% Define a macro SERVER that is replaced with ?MODULE (current module name)
-define(SERVER, ?MODULE).
-behaviour(gen_statem).
-compile(export_all).
-compile(nowarn_export_all).

% Function to start the state machine
start_link(BobPid) ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [BobPid], []).

callback_mode() -> [state_functions].

init([BobPid]) ->
  {ok, state1, {BobPid, []}, [{next_event, internal, {send_ping}}]}.


send_ping(BobPid) ->
  io:format("Alice: Sending ping to Bob~n"),
  gen_statem:cast(BobPid, ping).

start_sending() ->
  gen_statem:cast(?SERVER, {ping}).

state1(internal, {send_ping}, {BobPid, Data}) ->
  send_ping(BobPid),
  io:format("Alice: Switching to state2~n"),
  {next_state, state2, Data}.

state2(cast, {pong}, Data) ->
  io:format("ALice: Received pong from Bob~n"),
  io:format("Alice: Goodbye!~n"),
  {stop, normal, Data}.
