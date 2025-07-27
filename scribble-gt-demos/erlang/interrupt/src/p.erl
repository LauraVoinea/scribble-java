-module(p).
-behaviour(gen_p).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s4/3,
	 make_choice_s6/1,
	 s6/3,
	 s9/3
	]).

-include("p.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), q_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_p:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data(), [{next_event, internal, {'Start'}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("p initialized ~n", []),
    {ok, s4, Data, [{next_event, internal, {'Start'}}]}.

-spec s4(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s6, state_data()} |
    {stop, normal, state_data()}.
s4(internal, {'Start'}, Data) ->
    NewData = connection(Data),
    QPid = NewData#state_data.q_pid,
    io:format("P: s4 Sending Start to Q ~p~n", [QPid]),
    gen_p:send_s4_Start(QPid, NewData),
    case make_choice_s6(Data) of
        1 ->
            {next_state, s6, NewData, [{next_event, internal, {'Stop'}}]};
        2 ->
            {next_state, s6, NewData, [{next_event, internal, {'More'}}]}
    end;
s4(cast, {QPid, {'Interrupt'}}, Data) ->
    io:format("P: s4 Received Interrupt  from Q ~p ~n", [QPid]),
    {stop, normal, Data}.

-spec s6(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s9, state_data()} |
    {next_state, s6, state_data()} |
    {stop, normal, state_data()}.
s6(internal, {'Stop'}, #state_data{q_pid = QPid} = Data) ->
    io:format("P: s6 Sending Stop to Q ~n", []),
    gen_p:send_s6_Stop(QPid, Data),
    {next_state, s9, Data};
s6(internal, {'More'}, #state_data{q_pid = QPid} = Data) ->
    io:format("P: s6 Sending More to Q ~n", []),
    gen_p:send_s6_More(QPid, Data),
    case make_choice_s6(Data) of
        1 ->
            {next_state, s6, Data, [{next_event, internal, {'Stop'}}]};
        2 ->
            {next_state, s6, Data, [{next_event, internal, {'More'}}]}
    end;
s6(cast, {QPid, {'Interrupt'}}, #state_data{q_pid = QPid} = Data) ->
    io:format("P: s6 Received Interrupt  from Q ~p ~n", [QPid]),
    {stop, normal, Data}.

-spec make_choice_s6(state_data()) -> integer().
make_choice_s6(_Data) ->
    rand:uniform(2).

-spec s9(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s9(cast, {QPid, {'Ack'}}, #state_data{q_pid = QPid} = Data) ->
    io:format("P: s9 Received Ack  from Q ~p ~n", [QPid]),
    {stop, normal, Data};
s9(cast, {QPid, {'Interrupt'}}, #state_data{q_pid = QPid} = Data) ->
    io:format("P: s9 Received Interrupt  from Q ~p ~n", [QPid]),
    {stop, normal, Data}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("p connected ~n", []),
    QPid = case whereis(q) of
        undefined ->
            io:format("q is not available yet. Will retry...~n", []),
            timer:sleep(2000),
            whereis(q);
        Pid_q ->
            Pid_q
    end,
    Data#state_data{q_pid = QPid}.

