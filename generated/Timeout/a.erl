-module(a).
-behaviour(gen_a).

-export([init/1, callback_mode/0, start_link/0, s4/3, s5/3, s6/3]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_a:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data(), [{next_event, internal, {a1}}]}.
init([]) ->
    BPid = case whereis(b) of
        undefined ->
            io:format("b is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(b);
        Pid ->
            Pid
    end,
    BPid ! {a_pid, self()},
    CPid = case whereis(c) of
        undefined ->
            io:format("c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(c);
        Pid ->
            Pid
    end,
    CPid ! {a_pid, self()},
    Data = #state_data{mc_counter_1 = 0, b_pid = BPid, c_pid = CPid},
    io:format("a initialized ~n", []),
    {ok, s4, Data, [{next_event, internal, {a1}}]}.

-spec s4(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()}.
s4(internal, {a1}, #state_data{b_pid = BPid} = Data) ->
    io:format("B: s4 Sending a1 to B ~n", []),
    gen_a:send_s4_a1(BPid, Data),
    {next_state, s5, Data};
s4(cast, {BPid, {'To'}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data}.

-spec s5(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()} | {next_state, s6, state_data(), [{next_event, internal, {a6}}]}.
s5(cast, {BPid, {'To'}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data};
s5(cast, {BPid, {a5}}, #state_data{b_pid = BPid} = Data) ->
    {next_state, s6, Data, [{next_event, internal, {a6}}]}.

-spec s6(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(internal, {a6}, #state_data{c_pid = CPid} = Data) ->
    io:format("B: s6 Sending a6 to C ~n", []),
    gen_a:send_s6_a6(CPid, Data),
    {stop, normal, Data}.

