-module(a).
-behaviour(gen_a).

-export([init/1, callback_mode/0, start_link/0, make_choice_s5/1, s5/3, s6/3, s9/3]).

-include("a.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), b_pid :: pid() | undefined, prev_value :: integer(), curr_value :: integer()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_a:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s5, state_data()} | {next_state, s5, state_data(), [term()]}.
init([]) ->
    BPid = case whereis(b) of
        undefined ->
            io:format("b is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(b);
        Pid ->
            Pid
    end,
    % Initialize Fibonacci with prev=0 and curr=1
    Data = #state_data{mc_counter_1 = 0, b_pid = BPid, prev_value = 0, curr_value = 1},
    io:format("a initialized ~n", []),
    case make_choice_s5(Data) of
        1 ->
            {ok, s5, Data, [{next_event, internal, {fibonacci}}]};
        2 ->
            {ok, s5, Data, [{next_event, internal, {stop}}]}
    end.

-spec s5(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s6, state_data()} | {next_state, s9, state_data()} | {stop, normal, state_data()}.
s5(internal, {fibonacci}, Data) ->
    BPid = case Data#state_data.b_pid of
            undefined ->
                receive {b_pid, Pid1} when is_pid(Pid1) -> Pid1 end;
            Pid1 when is_pid(Pid1) ->
                Pid1
        end,
    NewData = Data#state_data{b_pid = BPid},
    io:format("A: s5 Sending fibonacci to b ~p~n", [NewData#state_data.curr_value]),
    gen_a:send_s5_fibonacci(BPid, NewData),
    {next_state, s6, NewData};
s5(internal, {stop}, Data) ->
    BPid = case Data#state_data.b_pid of
            undefined -> receive {b_pid, Pid1} -> Pid1 end;
            Pid1 -> Pid1
        end,
    NewData = Data#state_data{b_pid = BPid},
    io:format("B: s5 Sending stop to b ~n", []),
    gen_a:send_s5_stop(BPid, NewData),
    {next_state, s9, NewData};
s5(cast, {BPid, {error}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data}.

-spec s6(cast, {pid(), {atom(), integer()}}, state_data()) -> {ok, s5, state_data()} | {next_state, s5, state_data(), [term()]}.  % fixed semicolon to period
s6(cast, {BPid, {fibonacci, Num}}, #state_data{b_pid = BPid, curr_value = Curr} = Data) ->
    % Compute next Fibonacci number
    Next = Curr + Num,
    NewData = Data#state_data{prev_value = Curr, curr_value = Next},
    io:format("A: s6 Received ~p, next is ~p~n", [Num, Next]),
    case make_choice_s5(NewData) of
        1 -> {next_state, s5, NewData, [{next_event, internal, {fibonacci}}]};
        2 -> {next_state, s5, NewData, [{next_event, internal, {stop}}]}
    end.

-spec s9(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()}.
s9(cast, {BPid, {ack}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data};
s9(cast, {BPid, {error}}, #state_data{b_pid = BPid} = Data) ->
    {stop, normal, Data}.

%% Deterministic iteration-based choice: 1=fibonacci, 2=stop
-define(MAX_S5_ITER, 10).
-spec make_choice_s5(state_data()) -> integer().
make_choice_s5(#state_data{mc_counter_1 = Count}) -> %1.
    if
        Count < ?MAX_S5_ITER -> 1;
        true -> 2
    end.
