-module(a).
-behaviour(gen_a).

-export([init/1,
    callback_mode/0,
    start_link/0,
    s1/3
]).

-include("a.hrl").
-type state_data() :: #state_data{b_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_a:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {ping}}]}.
init([]) ->
    Data = #state_data{},
    io:format("a initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {ping}}]}.

-spec s1(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s1(internal, {ping}, Data) ->
    Data1 = connection(Data),
    BPid = Data1#state_data.b_pid,
    io:format("A: s1 Sending ping to B ~n", []),
    gen_a:send_s1_ping(BPid, Data),
    {stop, normal, Data}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("a connected ~n", []),
    BPid = case whereis(b) of
               undefined ->
                   io:format("b is not available yet. Will retry...~n", []),
                   timer:sleep(1000),
                   whereis(b);
               Pid_b ->
                   Pid_b
           end,
    CPid = case whereis(c) of
               undefined ->
                   io:format("c is not available yet. Will retry...~n", []),
                   timer:sleep(1000),
                   whereis(c);
               Pid_c ->
                   Pid_c
           end,
    Data#state_data{b_pid = BPid}.

