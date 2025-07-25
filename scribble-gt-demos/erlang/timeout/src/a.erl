-module(a).
-behaviour(gen_a).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s4/3,
	 s5/3,
	 s6/3,
	 s7/3
	]).

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
    Data = #state_data{mc_counter_1 = 0},
    io:format("a initialized ~n", []),
    {ok, s4, Data, [{next_event, internal, {a1}}]}.

-spec s4(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s5, state_data(), [{next_event, internal, {a2}}]} |
    {stop, normal, state_data()}.
s4(internal, {a1}, Data) ->
    NewData = connection(Data),
    BPid = NewData#state_data.b_pid,
    io:format("A: s4 Sending a1 to B ~n", []),
    gen_a:send_s4_a1(BPid, NewData),
    {next_state, s5, NewData, [{next_event, internal, {a2}}]};
s4(cast, {BPid, {'TOa'}}, Data) ->
    NewData = connection(Data),
    io:format("A: s4 Received TOa  from B ~p ~n", [BPid]),
    {stop, normal, NewData}.

-spec s5(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s6, state_data()} |
    {stop, normal, state_data()}.
s5(internal, {a2}, #state_data{c_pid = CPid} = Data) ->
    io:format("A: s5 Sending a2 to C ~n", []),
    gen_a:send_s5_a2(CPid, Data),
    {next_state, s6, Data};
s5(cast, {BPid, {'TOa'}}, #state_data{b_pid = BPid} = Data) ->
    io:format("A: s5 Received TOa  from B ~p ~n", [BPid]),
    {stop, normal, Data}.

-spec s6(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s7, state_data()} |
    {stop, normal, state_data()}.
s6(cast, {BPid, {a4}}, #state_data{b_pid = BPid} = Data) ->
    io:format("A: s6 Received a4  from B ~p ~n", [BPid]),
    {next_state, s7, Data};
s6(cast, {BPid, {'TOa'}}, #state_data{b_pid = BPid} = Data) ->
    io:format("A: s6 Received TOa  from B ~p ~n", [BPid]),
    {stop, normal, Data}.

-spec s7(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s7(cast, {CPid, {a5}}, #state_data{c_pid = CPid} = Data) ->
    io:format("C: s7 Received a5  from C ~p ~n", [CPid]),
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
    Data#state_data{b_pid = BPid, c_pid = CPid}.

