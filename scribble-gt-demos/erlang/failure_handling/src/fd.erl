-module(fd).
-behaviour(gen_fd).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 make_choice_Timeout/1,
	 s6/3,
	 make_choice_HB/1,
	 s7/3,
	 s4/3
	]).

-include("fd.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), w_pid :: pid() | undefined, m_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_fd:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s6, state_data(), [{next_event, internal, {'Timeout'}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("fd initialized ~n", []),
    {ok, s6, Data, [{next_event, internal, {'Timeout'}}]}.

-spec s4(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s4(internal, {'Crash'}, #state_data{m_pid = MPid} = Data) ->
    io:format("FD: s4 Sending Crash to M ~n", []),
    gen_fd:send_s4_Crash(MPid, Data),
    {stop, normal, Data}.

-spec make_choice_HB(state_data()) -> integer().
make_choice_HB(_Data) ->
    rand:uniform(2).

-spec s6(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s4, state_data(), [{next_event, internal, {'Crash'}}]} |
    {keep_state, state_data()} |
    {next_state, s7, state_data(), [{next_event, internal, {'OK'}}]}.
s6(internal, {'Timeout'}, Data) ->
    NewData = connection(Data),
    WPid = NewData#state_data.w_pid,
    case make_choice_Timeout(Data) of
        1 ->
            {keep_state, NewData};
        2 ->
            gen_fd:send_s6_Timeout(WPid, NewData),
            io:format("FD: s6 Sending Timeout to W ~n", []),
            {next_state, s4, NewData, [{next_event, internal, {'Crash'}}]}
    end;
s6(cast, {WPid, {'HB'}}, Data) ->
    io:format("FD: s6 Received HB  from W ~p ~n", [WPid]),
    case make_choice_HB(Data) of
        1 ->
            {next_state, s7, Data, [{next_event, internal, {'OK'}}]};
        2 ->
            gen_fd:send_s6_Timeout(WPid, Data),
            {next_state, s4, Data, [{next_event, internal, {'Crash'}}]}
    end.

-spec s7(internal, {atom()}, state_data()) -> {ok, s6, state_data(), [{next_event, internal, {'Timeout'}}]}.
s7(internal, {'OK'}, #state_data{m_pid = MPid} = Data) ->
    io:format("FD: s7 Sending OK to M ~n", []),
    gen_fd:send_s7_OK(MPid, Data),
    {next_state, s6, Data, [{next_event, internal, {'Timeout'}}]}.

-spec make_choice_Timeout(state_data()) -> integer().
make_choice_Timeout(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("fd connected ~n", []),
    WPid = case whereis(w) of
        undefined ->
            io:format("w is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(w);
        Pid_w ->
            Pid_w
    end,
    MPid = case whereis(m) of
        undefined ->
            io:format("m is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(m);
        Pid_m ->
            Pid_m
    end,
    Data#state_data{w_pid = WPid, m_pid = MPid}.

