-module(srv).
-behaviour(gen_srv).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, make_choice_timeout/1, s6/3, make_choice_sum/1, make_choice_diff/1, s7/3, s9/3]).

-include("srv.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_srv:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("srv initialized ~n", []),
    {ok, s1, Data}.

-spec connect(state_data()) -> state_data().
connect(Data) ->
    AlicePid = case whereis(alice) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(alice);
        Pid ->
            Pid
    end,
    CarolPid = case whereis(carol) of
        undefined ->
            io:format("carol is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(carol);
        Pid1 ->
            Pid1
    end,
    Data#state_data{carol_pid = CarolPid, alice_pid = AlicePid}.

-spec make_choice_sum(state_data()) -> integer().
make_choice_sum(_Data) ->
    rand:uniform(2).

-spec make_choice_timeout(state_data()) -> integer().
make_choice_timeout(_Data) ->
    rand:uniform(2).

-spec make_choice_diff(state_data()) -> integer().
make_choice_diff(_Data) ->
    rand:uniform(2).


-spec s1(cast, {pid(), {atom(), integer()}}, state_data()) -> {next_state, s3, state_data()}.
s1(cast, {_CarolPid, {first, Number}}, Data) ->
    NewData = connect(Data),
    io:format("Server: s1 Received first ~p from Carol ~n", [Number]),
    {next_state, s3, NewData}.

-spec s3(cast, {pid(), {atom(), integer()}}, state_data()) -> 
    {next_state, s6, state_data(), [{next_event, internal, {timeout}}]}.
s3(cast, {CarolPid, {second, Number}}, #state_data{carol_pid = CarolPid} = Data) ->
    io:format("Server: s3 Received second ~p from Carol ~n", [Number]),
    {next_state, s6, Data, [{next_event, internal, {timeout}}]}.

-spec s6(internal | EventType :: term(), {atom()} | {pid(), {atom()}}, state_data()) -> 
    {stop, normal, state_data()} | 
    {next_state, s7, state_data(), [{next_event, internal, {result_sum}}]} | 
    {next_state, s9, state_data(), [{next_event, internal, {result_diff}}]}|
    {keep_state, state_data()}.
s6(internal, {timeout}, #state_data{carol_pid = CarolPid} = Data) ->
    case make_choice_timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            io:format("Server: s6 Sending timeout to Carol ~n", []),
            gen_srv:send_s6_timeout(CarolPid, Data),
            {stop, normal, Data}
    end;
s6(cast, {CarolPid, {sum}}, #state_data{carol_pid = CarolPid} = Data) ->
    case make_choice_sum(Data) of
        1 ->
            {next_state, s7, Data, [{next_event, internal, {result_sum}}]};
        2 ->
            io:format("Server: s6 Sending timeout to Carol ~n", []),
            gen_srv:send_s6_timeout(CarolPid, Data),
            {stop, normal, Data}
    end;
s6(cast, {CarolPid, {diff}}, #state_data{carol_pid = CarolPid} = Data) ->
    case make_choice_diff(Data) of
        1 ->
            {next_state, s9, Data, [{next_event, internal, {result_diff}}]};
        2 ->
            io:format("Server: s6 Sending timeout to Carol ~n", []),
            gen_srv:send_s6_timeout(CarolPid, Data),
            {stop, normal, Data}
    end.

-spec s7(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(internal, {result_sum}, #state_data{carol_pid = CarolPid} = Data) ->
    io:format("Server: s7 Sending result_sum to Carol ~n", []),
    Result = 42, % Example result, can be replaced with actual logic
    gen_srv:send_s7_result_sum(CarolPid, Result, Data),
    {stop, normal, Data}.

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {result_diff}, #state_data{carol_pid = CarolPid} = Data) ->
    io:format("Server: s9 Sending result_diff to Carol ~n", []),
    Result = 42, % Example result, can be replaced with actual logic
    gen_srv:send_s9_result_diff(CarolPid, Result, Data),
    {stop, normal, Data}.


