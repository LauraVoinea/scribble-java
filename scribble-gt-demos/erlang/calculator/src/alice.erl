-module(alice).
-behaviour(gen_alice).

-export([init/1, callback_mode/0, start_link/0, s27/3]).

-include("alice.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, srv_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_alice:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s27, state_data()}.
init([]) ->
    CarolPid = case whereis(carol) of
        undefined ->
            io:format("carol is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(carol);
        Pid ->
            Pid
    end,
    CarolPid ! {alice_pid, self()},
    SrvPid = case whereis(srv) of
        undefined ->
            io:format("srv is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(srv);
        Pid ->
            Pid
    end,
    SrvPid ! {alice_pid, self()},
    Data = #state_data{mc_counter_1 = 0, carol_pid = CarolPid, srv_pid = SrvPid},
    io:format("alice initialized ~n", []),
    {ok, s27, Data}.

-spec s27(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()} | {stop, normal, state_data()}.
s27(cast, {CarolPid, {sum_result, result}}, #state_data{carol_pid = CarolPid} = Data) ->
    {stop, normal, Data};
s27(cast, {CarolPid, {diff_result, result}}, #state_data{carol_pid = CarolPid} = Data) ->
    {stop, normal, Data};
s27(cast, {CarolPid, {cancel}}, #state_data{carol_pid = CarolPid} = Data) ->
    {stop, normal, Data}.

