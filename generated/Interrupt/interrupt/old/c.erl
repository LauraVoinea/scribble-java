-module(c).
-behaviour(gen_c).

-export([start_link/0, init/1, state1/3, state2/3]).

-include("../c.hrl").

-type state_data() :: #state_data{}.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_c:start_link(?MODULE, []).

-spec init(list()) -> {ok, state1, state_data()}.
init([]) ->
  APid = case whereis(a) of
           undefined ->
             io:format("Process a not found, waiting...~n"),
             timer:sleep(1000),
             whereis(a);
           APid1 -> APid1
         end,
  BPid = case whereis(b) of
           undefined ->
             io:format("Process b not found, waiting...~n"),
             timer:sleep(1000),
             whereis(b);
           BPid1 -> BPid1
         end,
    io:format("c APid: ~p~n", [APid]),
    io:format("c BPid: ~p~n", [BPid]),
    {ok, state1, #state_data{a_pid = APid, b_pid = BPid}}.

-spec state1(atom(), {pid(), {2}} | {pid(), {exception}}, state_data()) -> {next_state, state1, state_data()} |
    {next_state, state1, state_data()} .
state1(cast, {_BPid, {2}}, Data) ->
    io:format("c: Receiving ~p~n", [2]),
    {next_state, state2, Data};
state1(cast, {_BPid, {exception}}, Data) ->
    io:format("c: Receiving exception~n"),
    {next_state, state4, Data}.

-spec state2(atom(), {pid(), {6}}, state_data()) -> {stop, normal, state_data()} .
state2(cast, {_BPid, {6}}, Data) ->
    {stop, normal, Data}.



