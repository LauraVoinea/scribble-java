-module(bob).
-define(SERVER, ?MODULE).
-behaviour(gen_statem).
-compile(export_all).

-record(state_data, {alice_pid}).

% Function to start the state machine
start_link() -> gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

callback_mode() -> [state_functions].

init([]) ->
  io:format("Waiting for Alice to send its PID...~n"),
  {ok, waiting_for_alice_pid, #state_data{}}.

send_pong(AlicePid) ->
%%  io:format("Bob: Sending pong to Alice~n"),
  gen_statem:cast(AlicePid, {pong}).

send_error(AlicePid) ->
  io:format("Bob: Sending error to Alice~n"),
  gen_statem:cast(AlicePid, {error}).

coin_toss() ->
  case rand:uniform(2) of
    1 -> {wait_for_ping};
    2 -> {error}
  end.

waiting_for_alice_pid(info, {alice_pid, AlicePid}, _Data) ->
  io:format("Bob: Received Alice's PID: ~p~n", [AlicePid]),
  {next_state, state1, #state_data{alice_pid = AlicePid}, [{next_event, internal, coin_toss}]}.

state1(internal, coin_toss, #state_data{alice_pid = AlicePid} = Data) ->
  case coin_toss() of
    {wait_for_ping} ->
      io:format("Bob: Coin toss: Waiting for ping from Alice~n"),
      {keep_state, Data};
    {error} ->
      io:format("Bob: Coin toss: Error, sending error to Alice~n"),
      send_error(AlicePid),
      io:format("Bob: Goodbye!~n"),
      {stop, normal, Data}
  end;

% Waiting for ping from Alice
state1(cast, {ping}, Data) ->
  io:format("Bob: Received ping from Alice~n"),
  {next_state, state3, Data, [{next_event, internal, {pong}}]}.

% Waiting for ping from Alice
waiting_for_ping(cast, {ping}, Data) ->
  io:format("Bob: Received ping from Alice~n"),
  {next_state, state3, Data, [{next_event, internal, {pong}}]}.

% State3: Send pong and transition back to state1
state3(internal, {pong}, #state_data{alice_pid = AlicePid} = Data) ->
  send_pong(AlicePid),
  io:format("Bob: Sending pong to Alice from State3~n"),
  {next_state, state1, Data, [{next_event, internal, coin_toss}]}.