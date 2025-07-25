-module(s).
-behaviour(gen_s).

-export([init/1, callback_mode/0, start_link/0, s1/3, s4/3, make_choice_timeout/1, s8/3, make_choice_pay/1, make_choice_quit/1, s9/3, s12/3, s6/3]).

-include("s.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_s:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("s initialized ~n", []),
    {ok, s1, Data}.

-spec make_choice_timeout(state_data()) -> integer().
make_choice_timeout(_Data) ->
    rand:uniform(2).

-spec s4(internal, {atom()}, state_data()) -> {next_state, s8, state_data(), [{next_event, internal, {timeout}}]}.
s4(internal, {account}, #state_data{c_pid = CPid} = Data) ->
    Balance = 1000, % Example balance
    Overdraft = 500, % Example overdraft
    io:format("S: s4 Sending account to C ~p ~p~n", [Balance, Overdraft]),
    gen_s:send_s4_account(CPid, Balance, Overdraft, Data),
    {next_state, s8, Data, [{next_event, internal, {timeout}}]}.

-spec s6(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(internal, {timeout}, #state_data{a_pid = APid} = Data) ->
    io:format("S: s6 Sending timeout to A ~n", []),
    gen_s:send_s6_timeout(APid, Data),
    {stop, normal, Data}.

-spec s12(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(internal, {quit_ack}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s12 Sending quit_ack to C ~n", []),
    gen_s:send_s12_quit_ack(CPid, Data),
    {stop, normal, Data}.

-spec make_choice_pay(state_data()) -> integer().
make_choice_pay(_Data) ->
    rand:uniform(2).

-spec s8(internal | EventType :: term(), {atom()} | {pid(), {atom(), term(), term()}}, state_data()) -> 
    {next_state, s6, state_data(), [{next_event, internal, {timeout}}]} | 
    {next_state, s9, state_data(), [{next_event, internal, {confirmation}}]} | 
    {next_state, s12, state_data(), [{next_event, internal, {quit_ack}}]} |
    {keep_state, state_data()}.
s8(internal, {timeout}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_timeout(Data) of
        1 ->
            io:format("S: s8 waiting for C ~n", []),
            {keep_state, Data};
        2 ->
            io:format("S: s8 Sending timeout to C ~n", []),
            gen_s:send_s8_timeout(CPid, Data),
            {next_state, s6, Data, [{next_event, internal, {timeout}}]}
    end;
s8(cast, {CPid, {pay, Payee, Amount}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s8 Received pay request from C ~p ~p ~n", [Payee, Amount]),
    case make_choice_pay(Data) of
        1 ->
            io:format("S: s8 choosing confirmation ~n", []),
            {next_state, s9, Data, [{next_event, internal, {confirmation}}]};
        2 ->
            io:format("S: s8 choosing timeout ~n", []),
            gen_s:send_s8_timeout(CPid, Data),
            {next_state, s6, Data, [{next_event, internal, {timeout}}]}
    end;
s8(cast, {CPid, {quit}}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_quit(Data) of
        1 ->
            {next_state, s12, Data, [{next_event, internal, {quit_ack}}]};
        2 ->
            gen_s:send_s8_timeout(CPid, Data),
            gen_s:send_s8_timeout(CPid, Data),
            {next_state, s6, Data, [{next_event, internal, {timeout}}]}
    end.

-spec s9(internal, {atom()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {account}}]}.
s9(internal, {confirmation}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s9 Sending confirmation to C ~n", []),
    gen_s:send_s9_confirmation(CPid, Data),
    {next_state, s4, Data, [{next_event, internal, {account}}]}.

-spec make_choice_quit(state_data()) -> integer().
make_choice_quit(_Data) ->
    rand:uniform(2).

-spec s1(cast, {pid() | undefined, {atom()} | {atom(), term()} }, state_data()) -> {
    next_state, s4, state_data(), [{next_event, internal, {account}}]} |
    {stop, normal, state_data()}.
s1(cast, {APid, {login_accepted}}, Data) ->
    io:format("S: s1 Received login_accepted from A ~p ~n", [APid]),
    Data1 = connection(Data),
    {next_state, s4, Data1, [{next_event, internal, {account}}]};
s1(cast, {APid, {auth_fail}}, Data) ->
    io:format("S: s1 Received auth_fail from A ~p ~n", [APid]),
    Data1 = connection(Data),
    {stop, normal, Data1}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    APid = case whereis(a) of
        undefined ->
            io:format("S: a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid_a ->
            Pid_a
    end,
    CPid = case whereis(client) of
        undefined ->
            io:format("S: c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(client);
        Pid_c ->
            Pid_c
    end,
    Data#state_data{a_pid = APid, c_pid = CPid}.

