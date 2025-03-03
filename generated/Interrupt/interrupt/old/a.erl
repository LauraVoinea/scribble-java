-module(a).
-behaviour(gen_a).

-export([start_link/0, init/1, state1/3, state2/3]).

-include("../a.hrl").

-type state_data() :: #state_data{}.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_a:start_link(?MODULE, []).

-spec init(list()) -> {ok, state1, state_data()}.
init([]) ->
    io:format("Process a initialized.~n"),
    {ok, state1, #state_data{}, [{next_event, internal, {1}}]}.

-spec state1(atom(), {1} | {pid(), {exception}}, state_data()) -> {next_state, state2, state_data()} |
    {stop, normal, state_data()} .
state1(internal, {1}, Data) ->
    io:format("Sending ~p to ~p ~n", [1, whereis(b)]),
    BPid = case whereis(b) of
               undefined ->
                   io:format("Process b not found, waiting...~n"),
                   timer:sleep(1000),
                   whereis(b);
               Pid -> Pid
           end,
    gen_a:send_1(BPid),
    {next_state, state2, Data};
state1(cast, {_BPid, {exception}}, Data) ->
    {stop, normal, Data}.

-spec state2(atom(), {pid(), {5}} | {pid(), {exception}}, state_data()) -> {next_state, state2, state_data()} | 
    {next_state, state2, state_data()} .
state2(cast, {_BPid, {5}}, Data) ->
    io:format("a: Received ~p~n", [5]),
    {stop, normal, Data};
state2(cast, {_BPid, {exception}}, Data) ->
    io:format("a: Received exception~n"),
    {stop, normal, Data}.



