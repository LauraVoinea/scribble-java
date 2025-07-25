-module(alice).
-behaviour(gen_alice).

-export([init/1, callback_mode/0, start_link/0, s4/3]).

-include("alice.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, srv_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_alice:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("alice initialized ~n", []),
    {ok, s4, Data}.


-spec connect(state_data()) -> state_data().
connect(Data) ->
    SrvPid = case whereis(srv) of
        undefined ->
            io:format("srv is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(srv);
        Pid ->
            Pid
    end,

    CarolPid = case whereis(carol) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(carol);
        Pid1 ->
            Pid1
    end,
    Data#state_data{srv_pid = SrvPid, carol_pid = CarolPid}.

-spec s4(cast, {pid(), {atom(), integer()} | {atom()}}, state_data()) ->
    {stop, normal, state_data()}.
s4(cast, {_CarolPid, {diff_result, Result}}, Data) ->
    Data1 = connect(Data),
    io:format("Alice: s4 Received diff_result ~p from Carol ~n", [Result]),
    {stop, normal, Data1};
s4(cast, {_CarolPid, {sum_result, Result}}, Data) ->
    Data1 = connect(Data),
    io:format("Alice: s4 Received sum_result ~p from Carol ~n", [Result]),
    {stop, normal, Data1};
s4(cast, {_CarolPid, {cancel}}, Data) ->
    Data1 = connect(Data),
    io:format("Alice: s4 Received cancel from Carol ~n", []),
    {stop, normal, Data1}.

