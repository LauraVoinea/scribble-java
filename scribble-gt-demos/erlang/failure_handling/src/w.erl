-module(w).
-behaviour(gen_w).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 s6/3,
	 s7/3,
	 s8/3
	]).

-include("w.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), m_pid :: pid() | undefined, fd_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_w:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("w initialized ~n", []),
    {ok, s1, Data}.

-spec s6(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s7, state_data()} |
    {stop, normal, state_data()}.
s6(internal, {'HB'}, #state_data{fd_pid = FDPid} = Data) ->
    case make_choice_s6()  of
          1 ->
              exit(simulated_crash);
          _ ->     io:format("W: s6 Sending HB to FD ~n", []),
              gen_w:send_s6_HB(FDPid, Data),
              {next_state, s7, Data, [{next_event, internal, {result}}]}
    end;
s6(cast, {FDPid, {'Timeout'}}, #state_data{fd_pid = FDPid} = Data) ->
    io:format("W: s6 Received Timeout  from FD ~p ~n", [FDPid]),
    {stop, normal, Data}.

-spec s7(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s8, state_data()} |
    {stop, normal, state_data()}.
s7(internal, {result}, #state_data{m_pid = MPid} = Data) ->
    io:format("W: s7 Sending result to M ~n", []),
    gen_w:send_s7_result(MPid, result, Data),
    {next_state, s8, Data};
s7(cast, {FDPid, {'Timeout'}}, #state_data{fd_pid = FDPid} = Data) ->
    io:format("W: s7 Received Timeout  from FD ~p ~n", [FDPid]),
    {stop, normal, Data}.

-spec s8(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s6, state_data(), [{next_event, internal, {'HB'}}]} |
    {keep_state, state_data()} |
    {stop, normal, state_data()}.
s8(cast, {MPid, {more, Payload}}, #state_data{m_pid = MPid} = Data) ->
    io:format("W: s8 Received more Payload ~p from M ~p ~n", [Payload, MPid]),
    {next_state, s6, Data, [{next_event, internal, {'HB'}}]};
s8(cast, {FDPid, {'Timeout'}}, Data) ->
    io:format("W: s8 Received Timeout  from FD ~p ~n", [FDPid]),
    {stop, normal, Data}.

-spec s1(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s6, state_data(), [{next_event, internal, {'HB'}}]} |
    {keep_state, state_data()}.
s1(cast, {MPid, {init, Payload}}, Data) ->
    NewData = connection(Data),
    io:format("W: s1 Received init Payload ~p from M ~p ~n", [Payload, MPid]),
    {next_state, s6, NewData, [{next_event, internal, {'HB'}}]}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("w connected ~n", []),
    MPid = case whereis(m) of
        undefined ->
            io:format("m is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(m);
        Pid_m ->
            Pid_m
    end,
    FdPid = case whereis(fd) of
        undefined ->
            io:format("fd is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(fd);
        Pid_fd ->
            Pid_fd
    end,
    Data#state_data{m_pid = MPid, fd_pid = FdPid}.

make_choice_s6() ->
    %% Simulate a crash with a 10% probability
    case rand:uniform(10) of
        1 -> 1;  % Simulate crash
        _ -> 2   % Do not crash
    end.