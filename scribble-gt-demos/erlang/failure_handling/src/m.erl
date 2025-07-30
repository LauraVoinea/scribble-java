-module(m).
-behaviour(gen_m).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 s6/3,
	 s7/3,
	 s8/3
	]).

-include("m.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), w_pid :: pid() | undefined, fd_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_m:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {init}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("m initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {init}}]}.

-spec s6(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s7, state_data()} |
    {stop, normal, state_data()}.
s6(cast, {FDPid, {'OK'}}, Data) ->
    io:format("M: s6 Received OK  from FD ~p ~n", [FDPid]),
    {next_state, s7, Data};
s6(cast, {FDPid, {'Crash'}}, Data) ->
    io:format("M: s6 Received Crash  from FD ~p ~n", [FDPid]),
    {stop, normal, Data}.

-spec s7(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s8, state_data(), [{next_event, internal, {more}}]} |
    {keep_state, state_data()}.
s7(cast, {WPid, {result, Payload}}, #state_data{w_pid = WPid} = Data) ->
    io:format("M: s7 Received result Payload ~p from W ~p ~n", [Payload, WPid]),
    {next_state, s8, Data, [{next_event, internal, {more}}]}.

-spec s8(internal, {atom()}, state_data()) -> {next_state, s6, state_data()}.
s8(internal, {more}, #state_data{w_pid = WPid} = Data) ->
    io:format("M: s8 Sending more to W ~n", []),
    gen_m:send_s8_more(WPid, more, Data),
    {next_state, s6, Data}.

-spec s1(internal, {atom()}, state_data()) -> {next_state, s6, state_data()}.
s1(internal, {init}, Data) ->
    NewData = connection(Data),
    WPid = NewData#state_data.w_pid,
    io:format("M: s1 Sending init to W ~n", []),
    gen_m:send_s1_init(WPid, init, Data),
    {next_state, s6, NewData}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("m connected ~n", []),
    WPid = case whereis(w) of
        undefined ->
            io:format("w is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(w);
        Pid_w ->
            Pid_w
    end,
    FdPid = case whereis(fd) of
        undefined ->
            io:format("fd is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(fd);
        Pid_fd ->
            Pid_fd
    end,
    Data#state_data{w_pid = WPid, fd_pid = FdPid}.

