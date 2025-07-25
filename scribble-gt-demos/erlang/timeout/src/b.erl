-module(b).
-behaviour(gen_b).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 make_choice_TOa/1,
	 s5/3,
	 make_choice_a1/1,
	 s6/3,
	 s7/3,
	 s3/3
	]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s5, state_data(), [{next_event, internal, {'TOa'}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("b initialized ~n", []),
    {ok, s5, Data, [{next_event, internal, {'TOa'}}]}.

-spec s3(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(internal, {'TOc'}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s3 Sending TOc to C ~n", []),
    gen_b:send_s3_TOc(CPid, Data),
    {stop, normal, Data}.

s5(internal, {'TOa'}, Data) ->
    NewData = connection(Data),
    APid = NewData#state_data.a_pid,
    case make_choice_TOa(NewData) of
        1 ->
            {keep_state, NewData};
        2 ->
            gen_b:send_s5_TOa(APid, NewData),
            io:format("B: s5 Sending TOa to A ~n", []),
            {next_state, s3, NewData, [{next_event, internal, {'TOc'}}]}
    end;
s5(cast, {APid, {a1}}, #state_data{a_pid = APid} = Data) ->
    io:format("C: s5 Received a1  from A ~p ~n", [APid]),
    case make_choice_a1(Data) of
        1 ->
            {next_state, s6, Data, [{next_event, internal, {a3}}]};
        2 ->
            gen_b:send_s5_TOa(APid, Data),
            {next_state, s3, Data, [{next_event, internal, {'TOc'}}]}
    end.

s6(internal, {a3}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s6 Sending a3 to C ~n", []),
    gen_b:send_s6_a3(CPid, Data),
    {next_state, s7, Data, [{next_event, internal, {a4}}]}.

-spec s7(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(internal, {a4}, #state_data{a_pid = APid} = Data) ->
    io:format("B: s7 Sending a4 to A ~n", []),
    gen_b:send_s7_a4(APid, Data),
    {stop, normal, Data}.

-spec make_choice_TOa(state_data()) -> integer().
make_choice_TOa(_Data) ->
    rand:uniform(2).

-spec make_choice_a1(state_data()) -> integer().
make_choice_a1(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("b connected ~n", []),
    APid = case whereis(a) of
        undefined ->
            io:format("a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid_a ->
            Pid_a
    end,
    CPid = case whereis(c) of
        undefined ->
            io:format("c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(c);
        Pid_c ->
            Pid_c
    end,
    Data#state_data{a_pid = APid, c_pid = CPid}.

