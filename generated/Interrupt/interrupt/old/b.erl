-module(b).
-behaviour(gen_b).

-export([start_link/0, init/1, state1/3, state2/3, state3/3, state4/3, state6/3]).

-include("../b.hrl").

-type state_data() :: #state_data{}.

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
  gen_b:start_link(?MODULE, []).

-spec init(list()) -> {ok, state1, state_data()}.
init([]) ->
    {ok, state1, #state_data{}, [{next_event, internal, exception_choice}]}.

-spec state1(atom(), {pid(), {1}} |  exception_choice, state_data()) -> {next_state, state1, state_data()} | 
    {keep_state, state_data()} | 
    {next_state, state1, state_data()} .
state1(cast, {APid, {1}}, _Data) ->
    io:format("b: Received ~p from a ~n", [1]),
    {next_state, state2, #state_data{a_pid = APid}, [{next_event, internal, {2}}]};
state1(internal, exception_choice, Data) ->
    Choice = rand:uniform(2),
    io:format("b: Making a choice ~p~n", [Choice]),
    case Choice of
        1 ->
            io:format("b: Keeping state~n"),
            {keep_state, Data};
        2 ->
            io:format("b: Sending ~p to a ~n", [exception]),
            APid = whereis(a),
            gen_b:send_exception(APid),
            {next_state, state6, #state_data{a_pid = APid}, [{next_event, internal, {exception}}]}
        end.

-spec state2(atom(), {2}, state_data()) -> {next_state, state2, state_data()} .
state2(internal, {2}, Data) ->
    io:format("b: Sending ~p~n", [2]),
    CPid = whereis(c),
    gen_b:send_2(CPid),
    {next_state, state3, Data, [{next_event, internal, {5}}]}.

-spec state3(atom(), {5}, state_data()) -> {next_state, state3, state_data()} .
state3(internal, {5}, Data) ->
    io:format("b: Sending ~p~n", [5]),
    APid = whereis(a),
    gen_b:send_5(APid),
    {next_state, state4, Data, [{next_event, internal, {6}}]}.

-spec state4(atom(), {6}, state_data()) -> {stop, normal, state_data()} .
state4(internal, {6}, Data) ->
    io:format("b: Sending ~p to c ~n", [6]),
    CPid = whereis(c),
    gen_b:send_6(CPid),
    {stop, normal, Data}.


-spec state6(atom(), {exception}, state_data()) -> {stop, normal, state_data()} .
state6(internal, {exception}, #state_data{c_pid = CPid} = Data) ->
    io:format("b: Sending ~p to c ~n", [exception]),
    gen_b:send_exception(CPid),
    {stop, normal, Data}.


