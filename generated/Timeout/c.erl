-module(c).
-behaviour(gen_c).

-export([init/1, callback_mode/0, start_link/0, s4/3, s5/3]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, b_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_c:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("c initialized ~n", []),
    {ok, s4, Data}.

-spec s4(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
s4(cast, {BPid, {a2}}, #state_data{b_pid = BPid} = Data) ->
    {next_state, s5, Data};
s4(cast, {BPid, {'To'}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data}.

-spec s5(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s5(cast, {APid, {a6}}, #state_data{a_pid = APid} = Data) ->
    {stop, normal, Data}.

-spec connection() -> {state_data()}.
connection() ->
    io:format("c connected ~n", []),
    APid = case whereis(a) of
        undefined ->
            io:format("a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid_a ->
            Pid_a
    end,
    BPid = case whereis(b) of
        undefined ->
            io:format("b is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(b);
        Pid_b ->
            Pid_b
    end,
    #state_data{a_pid = APid, b_pid = BPid}.

