-module(a).
-behaviour(gen_a).

-export([init/1, callback_mode/0, start_link/0, s1/3, make_choice_s3/1, s3/3, s4/3, s8/3, s12/3]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), c_pid :: pid() | undefined, s_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_a:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("a initialized ~n", []),
    {ok, s1, Data}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s4, state_data(), [{next_event, internal, {login_accepted}}]} | {next_state, s12, state_data(), [{next_event, internal, {auth_fail}}]}.
s3(internal, {login_success}, #state_data{c_pid = CPid} = Data) ->
    io:format("A: s3 Sending login_success to C ~n", []),
    gen_a:send_s3_login_success(CPid, Data),
    {next_state, s4, Data, [{next_event, internal, {login_accepted}}]};
s3(internal, {login_failed}, #state_data{c_pid = CPid} = Data) ->
    io:format("A: s3 Sending login_failed to C ~n", []),
    gen_a:send_s3_login_failed(CPid, Data),
    {next_state, s12, Data, [{next_event, internal, {auth_fail}}]}.

-spec s4(internal, {atom()}, state_data()) -> {next_state, s8, state_data()}.
s4(internal, {login_accepted}, #state_data{s_pid = SPid} = Data) ->
    io:format("A: s4 Sending login_accepted to S ~n", []),
    gen_a:send_s4_login_accepted(SPid, Data),
    {next_state, s8, Data}.

-spec s12(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(internal, {auth_fail}, #state_data{s_pid = SPid} = Data) ->
    io:format("A: s12 Sending auth_fail to S ~n", []),
    gen_a:send_s12_auth_fail(SPid, Data),
    {stop, normal, Data}.

-spec s8(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s8, state_data()} | 
    {stop, normal, state_data()} | 
    {stop, normal, state_data()}.
s8(cast, {CPid, {keep_alive}}, #state_data{c_pid = CPid} = Data) ->
    io:format("A: s8 Received keep_alive from C ~n", []),
    {next_state, s8, Data};
s8(cast, {CPid, {end_session}}, #state_data{c_pid = CPid} = Data) ->
    io:format("A: s8 Received end_session from C ~n", []),
    {stop, normal, Data};
s8(cast, {SPid, {timeout}}, #state_data{s_pid = SPid} = Data) ->
    io:format("A: s8 Received timeout from S ~n", []),
    {stop, normal, Data}.

-spec make_choice_s3(state_data()) -> integer().
make_choice_s3(_Data) ->
    rand:uniform(2).

-spec s1(cast, {pid(), {atom(), term(), term()}}, state_data()) -> 
    {next_state, s3, state_data()} | 
    {next_state, s3, state_data(), [term()]}.
s1(cast, {_CPid, {login, Id, Password}}, Data) ->
    io:format("A: s1 Received login request from C ~p ~p ~n", [Id, Password]),
    Data1 = connection(Data),
    case make_choice_s3(Data) of
        1 ->
            {next_state, s3, Data1, [{next_event, internal, {login_success}}]};
        2 ->
            {next_state, s3, Data1, [{next_event, internal, {login_failed}}]}
    end.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("a connected ~n", []),
    CPid = case whereis(client) of
        undefined ->
            io:format("c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(client);
        Pid_c ->
            Pid_c
    end,
    SPid = case whereis(s) of
        undefined ->
            io:format("s is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(s);
        Pid_s ->
            Pid_s
    end,
    if
        CPid =:= undefined ->
            io:format("c is still not available after retrying. Exiting...~n", []),
            exit({error, no_c_pid});
        SPid =:= undefined ->
            io:format("s is still not available after retrying. Exiting...~n", []),
            exit({error, no_s_pid});
        true ->
            Data#state_data{c_pid = CPid, s_pid = SPid}

    end.
    
