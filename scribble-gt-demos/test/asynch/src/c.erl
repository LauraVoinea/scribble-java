-module(c).
-behaviour(gen_c).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3
	]).

-include("c.hrl").
-type state_data() :: #state_data{a_pid :: pid() | undefined, b_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_c:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {pong}}]}.
init([]) ->
    Data = #state_data{},
    io:format("c initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {pong}}]}.

-spec s1(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s1(internal, {pong}, Data) ->
    Data1 = connection(Data),
    BPid = Data1#state_data.b_pid,
    io:format("C: s1 Sending pong to B ~n", []),
    gen_c:send_s1_pong(BPid, Data1),
    {stop, normal, Data1}.

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
    BPid = case whereis(b) of
        undefined ->
            io:format("b is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(b);
        Pid_b ->
            Pid_b
    end,
    Data#state_data{a_pid = APid, b_pid = BPid}.

