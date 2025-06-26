-module(c).
-behaviour(gen_c).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, s5/3, make_choice_s8/1, s8/3, s9/3, s10/3, s13/3, s14/3]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, s_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_c:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {login}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("c initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {login}}]}.

-spec s3(cast, {pid(), {atom()}}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
s3(cast, {APid, {login_success}}, #state_data{a_pid = APid} = Data) ->
    {next_state, s5, Data};
s3(cast, {APid, {login_failed}}, #state_data{a_pid = APid} = Data) ->
    {stop, normal, Data}.

-spec s5(cast, {pid(), {atom(), term(), term()}}, state_data()) -> {next_state, s8, state_data()} | {next_state, s8, state_data(), [term()]}.
s5(cast, {SPid, {account, Balance, Overdraft}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s5 Received account from S balance: ~p; overdraft: ~p~n", [Balance, Overdraft]),
    case make_choice_s8(Data) of
        1 ->
            io:format("C: s5 Choosing pay ~n", []),
            {next_state, s8, Data, [{next_event, internal, {pay}}]};
        2 ->
            io:format("C: s5 Choosing quit ~n", []),
            {next_state, s8, Data, [{next_event, internal, {quit}}]}
    end.

-spec s10(internal, {atom()}, state_data()) -> {next_state, s5, state_data()}.
s10(internal, {keep_alive}, #state_data{a_pid = APid} = Data) ->
    io:format("C: s10 Sending keep_alive to A ~n", []),
    gen_c:send_s10_keep_alive(APid, Data),
    {next_state, s5, Data}.

-spec s13(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s14, state_data(), [{next_event, internal, {end_session}}]} | 
    {stop, normal, state_data()}.
s13(cast, {SPid, {quit_ack}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s13 Received quit_ack from S ~n", []),
    {next_state, s14, Data, [{next_event, internal, {end_session}}]};
s13(cast, {SPid, {timeout}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s13 Received timeout from S ~n", []),
    {stop, normal, Data}.

-spec s8(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s13, state_data()} | {next_state, s9, state_data()} | {stop, normal, state_data()}.
s8(internal, {quit}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s8 Sending quit to S ~n", []),
    gen_c:send_s8_quit(SPid, Data),
    {next_state, s13, Data};
s8(internal, {pay}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s8 Sending pay to S ~n", []),
    Amount = 100, % Example amount
    Payee = "Alice", % Example payee
    io:format("C: s8 Sending pay to S with Amount: ~p, Payee: ~p~n", [Amount, Payee]),
    gen_c:send_s8_pay(SPid, Payee, Amount, Data),
    {next_state, s9, Data};
s8(cast, {SPid, {timeout}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s9(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s10, state_data(), [{next_event, internal, {keep_alive}}]} | 
    {stop, normal, state_data()}.
s9(cast, {SPid, {confirmation}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s9 Received confirmation from S ~n", []),
    {next_state, s10, Data, [{next_event, internal, {keep_alive}}]};
s9(cast, {SPid, {timeout}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s9 Received timeout from S ~n", []),
    {stop, normal, Data}.

-spec make_choice_s8(state_data()) -> integer().
make_choice_s8(_Data) ->
    rand:uniform(2).

-spec s14(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s14(internal, {end_session}, #state_data{a_pid = APid} = Data) ->
    io:format("C: s14 Sending end_session to A ~n", []),
    gen_c:send_s14_end_session(APid, Data),
    {stop, normal, Data}.

-spec s1(internal, {atom()}, state_data()) -> {next_state, s3, state_data()}.
s1(internal, {login}, Data) ->
    io:format("C: s1 Sending login to A ~n", []),
    Data1 = connection(Data),
    APid = Data1#state_data.a_pid,
    Id = "user",
    Password = "password",
    io:format("C: s1 Sending login with Id: ~p, Password: ~p~n", [Id, Password]),
    gen_c:send_s1_login(APid, Id, Password, Data),
    {next_state, s3, Data1}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("c connected ~n", []),
    APid = case whereis(a) of
        undefined ->
            io:format("a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid_a ->
            Pid_a
    end,
    SPid = case whereis(s) of
        undefined ->
            io:format("s is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(s);
        Pid_s ->
            Pid_s
    end,
    Data#state_data{a_pid = APid, s_pid = SPid}.

