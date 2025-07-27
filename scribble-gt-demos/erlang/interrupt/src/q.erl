-module(q).
-behaviour(gen_q).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 make_choice_Interrupt/1,
	 s4/3,
	 make_choice_Start/1,
	 s6/3,
	 s9/3
	]).

-include("q.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), p_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_q:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data(), [{next_event, internal, {'Interrupt'}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("q initialized ~n", []),
    {ok, s4, Data, [{next_event, internal, {'Interrupt'}}]}.

-spec s4(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s6, state_data()}.
s4(internal, {'Interrupt'}, Data) ->
    NewData = connection(Data),
    PPid = NewData#state_data.p_pid,
    io:format("Q: s4 Sending Interrupt to P ~p~n", [PPid]),
    case make_choice_Interrupt(Data) of
        1 ->
            {keep_state, NewData};
        2 ->
            gen_q:send_s4_Interrupt(PPid, NewData),
            io:format("Q: s4 Sending Interrupt to P ~n", []),
            {stop, normal, NewData}
    end;
s4(cast, {PPid, {'Start'}}, Data) ->
    io:format("Q: s4 Received Start  from P ~p ~n", [PPid]),
    case make_choice_Start(Data) of
        1 ->
            {next_state, s6, Data};
        2 ->
            gen_q:send_s4_Interrupt(PPid, Data),
            {stop, normal, Data}
    end.

-spec s6(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s9, state_data(), [{next_event, internal, {'Ack'}}]} |
    {keep_state, state_data()} |
    {next_state, s6, state_data()}.
s6(cast, {PPid, {'Stop'}}, #state_data{p_pid = PPid} = Data) ->
    io:format("Q: s6 Received Stop  from P ~p ~n", [PPid]),
    {next_state, s9, Data, [{next_event, internal, {'Ack'}}]};
s6(cast, {PPid, {'More'}}, #state_data{p_pid = PPid} = Data) ->
    io:format("Q: s6 Received More  from P ~p ~n", [PPid]),
    {next_state, s6, Data}.

-spec make_choice_Start(state_data()) -> integer().
make_choice_Start(_Data) ->
    rand:uniform(2).

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {'Ack'}, #state_data{p_pid = PPid} = Data) ->
    io:format("Q: s9 Sending Ack to P ~n", []),
    gen_q:send_s9_Ack(PPid, Data),
    {stop, normal, Data}.

-spec make_choice_Interrupt(state_data()) -> integer().
make_choice_Interrupt(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("q connected ~n", []),
    PPid = case whereis(p) of
        undefined ->
            io:format("p is not available yet. Will retry...~n", []),
            timer:sleep(2000),
            whereis(p);
        Pid_p ->
            Pid_p
    end,
    Data#state_data{p_pid = PPid}.

