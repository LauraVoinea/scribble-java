-module(b).
-behaviour(gen_b).

-export([init/1, callback_mode/0, start_link/0, make_choice_To/1, s5/3, make_choice_a1/1, s6/3, s7/3, s3/3]).

-include("b.hrl").

%% Contains callback functions implementing custom logic
%% Defines application-specific behaviour (timeouts, conditions)
%% User-customisable; provides logic using the gen_role framework

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s5, state_data(), [{next_event, internal, {'To'}}]}.
init([]) ->
    APid = case whereis(a) of
        undefined ->
            io:format("B: a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid ->
            Pid
    end,
%%    APid ! {b_pid, self()},
    CPid = case whereis(c) of
        undefined ->
            io:format("B: c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(c);
        Pid1 ->
            Pid1
    end,
%%    CPid ! {b_pid, self()},
    Data = #state_data{mc_counter_1 = 0, a_pid = APid, c_pid = CPid},
    io:format("B: b initialized ~n", []),
    {ok, s5, Data, [{next_event, internal, {'To'}}]}.

-spec s3(internal, {'To'}, state_data()) -> {stop, normal, state_data()}.
s3(internal, {'To'}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s3 Sending To to C ~n", []),
    gen_b:send_s3_To(CPid, Data),
    {stop, normal, Data}.

-spec s5(internal | cast, {'To'} | {pid(), {a1}}, state_data()) ->
    {next_state, s3, state_data(), [{next_event, internal, {'To'}}]} |
    {next_state, s6, state_data(), [{next_event, internal, {a2}}]}.
s5(internal, {'To'}, Data) ->
    APid = case Data#state_data.a_pid of
                   undefined -> receive {a_pid, Pid} -> Pid end;
                   Pid -> Pid
               end,
    CPid = case Data#state_data.c_pid of
               undefined -> receive {c_pid, Pid1} -> Pid1 end;
               Pid1 -> Pid1
           end,
    NewData = Data#state_data{a_pid = APid, c_pid = CPid},

    case make_choice_To(NewData) of
        1 ->
            {keep_state, NewData};
        2 ->
            io:format("B: s5 Sending To to A ~p ~n", [Data]),
            gen_b:send_s5_To(APid, Data),
            {next_state, s3, NewData, [{next_event, internal, {'To'}}]}
    end;
s5(cast, {APid, {a1}}, #state_data{a_pid = APid} = Data) ->
    case make_choice_a1(Data) of
        1 ->
            io:format("B: s5 received a1 ~p ~n", [Data]),
            {next_state, s6, Data, [{next_event, internal, {a2}}]};
        2 ->
            io:format("B: s5 received a1, going to state3 To ~p ~n", [Data]),
            gen_b:send_s5_To(APid, Data),
            {next_state, s3, Data, [{next_event, internal, {'To'}}]}
    end.

-spec s6(internal, {a2}, state_data()) ->
    {next_state, s7, state_data(), [{next_event, internal, {a5}}]}.
s6(internal, {a2}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s6 sending a2 ~n", []),
    gen_b:send_s6_a2(CPid, Data),
    {next_state, s7, Data, [{next_event, internal, {a5}}]}.

-spec s7(internal, {a5}, state_data()) -> {stop, normal, state_data()}.
s7(internal, {a5}, #state_data{a_pid = APid} = Data) ->
    io:format("B: sending a5 ~n", []),
    gen_b:send_s7_a5(APid, Data),
    {stop, normal, Data}.

-spec make_choice_To(state_data()) -> integer().
make_choice_To(_Data) ->
    rand:uniform(2).

-spec make_choice_a1(state_data()) -> integer().
make_choice_a1(_Data) ->
    rand:uniform(2).
