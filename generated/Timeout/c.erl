-module(c).
-behaviour(gen_c).

-export([init/1, callback_mode/0, start_link/0, s1/3, s2/3]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, b_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_c:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    APid = case whereis(a) of
        undefined ->
            io:format("a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid ->
            Pid
    end,
    APid ! {c_pid, self()},
    BPid = case whereis(b) of
        undefined ->
            io:format("b is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(b);
        Pid ->
            Pid
    end,
    BPid ! {c_pid, self()},
    Data = #state_data{mc_counter_1 = 0, a_pid = APid, b_pid = BPid},
    io:format("c initialized ~n", []),
    {ok, s1, Data}.

-spec s1(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s2, state_data()} | {stop, normal, state_data()}.
s1(cast, {BPid, {a2}}, #state_data{b_pid = BPid} = Data) ->
    {next_state, s2, Data};
s1(cast, {BPid, {'Timeout'}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data}.

-spec s2(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s2(cast, {APid, {a6}}, #state_data{a_pid = APid} = Data) ->
    {stop, normal, Data}.

