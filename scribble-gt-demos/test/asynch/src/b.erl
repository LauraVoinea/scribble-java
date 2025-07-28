-module(b).
-behaviour(gen_b).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 s3/3
	]).

-include("b.hrl").
-type state_data() :: #state_data{a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{},
    io:format("b initialized ~n", []),
    {ok, s1, Data}.

%% s1 handles ping by transitioning and pong by postponing;
%% pong becomes a post-event that will be handled in s3 and
%% has priority over the next event in the queue.
-spec s1(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s3, state_data()} | {keep_state, state_data(), [postpone]}.
s1(cast, {APid, {ping}}, Data) ->
    io:format("b: ping received in s1, connecting~n", []),
    Data1 = connection(Data),
    {next_state, s3, Data1}.

-spec s3(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s3(cast, {CPid, {pong}}, Data) ->
    io:format("b: pong received in s3~n", []),
    {stop, normal, Data}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("b connected ~n", []),
    APid = case whereis(a) of
        undefined ->
            io:format("a is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(a);
        Pid_a ->
            Pid_a
    end,
    CPid = case whereis(c) of
        undefined ->
            io:format("c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(c);
        Pid_c ->
            Pid_c
    end,
    Data#state_data{a_pid = APid, c_pid = CPid}.
