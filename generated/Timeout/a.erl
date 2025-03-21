-module(a).
-behaviour(gen_a).

-export([init/1, callback_mode/0, start_link/0, s1/3, s2/3, s3/3]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_a:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {a1}}]}.
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
    {ok, s1, Data, [{next_event, internal, {a1}}]}.

-spec s3(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(internal, {a6}, #state_data{c_pid = CPid} = Data) ->
    gen_a:send_a6(CPid, a6),
    {stop, normal, Data}.

-spec s1(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s2, state_data()} | {stop, normal, state_data()}.
s1(internal, {a1}, #state_data{b_pid = BPid} = Data) ->
    gen_a:send_a1(BPid, a1),
    {next_state, s2, Data};
s1(cast, {BPid, {'Timeout'}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data}.

-spec s2(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()} | {next_state, s3, state_data(), [{next_event, internal, {a6}}]}.
s2(cast, {BPid, {'Timeout'}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data};
s2(cast, {BPid, {a5}}, #state_data{b_pid = BPid} = Data) ->
    {next_state, s3, Data, [{next_event, internal, {a6}}]}.

