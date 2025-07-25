-module(b).
-behaviour(gen_b).

-export([init/1, callback_mode/0, start_link/0, make_choice_error/1, s5/3, make_choice_stop/1, make_choice_fibonacci/1, s6/3, s9/3]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, prev_value :: integer(), curr_value :: integer()}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s5, state_data(), [{next_event, internal, {error}}]}.
init([]) ->
    APid = case whereis(a) of
               undefined ->
                   io:format("a is not available yet. Will retry...~n", []),
                   timer:sleep(1000),
                   whereis(a);
               Pid ->
                   Pid
           end,
    APid ! {b_pid, self()},
    % Initialize Fibonacci state with prev=0 and curr=0
    Data = #state_data{mc_counter_1 = 0, a_pid = APid, prev_value = 0, curr_value = 0},
    io:format("b initialized ~n", []),
    {ok, s5, Data, [{next_event, internal, {error}}]}.

-spec make_choice_fibonacci(state_data()) -> integer().
make_choice_fibonacci(_Data) -> 1.
% For demonstration purposes, we always choose to send fibonacci.
    % rand:uniform(2).

-spec s5(internal | EventType :: term(),
         {atom()}                
         | {pid(), {term()}}      
         | {pid(), {term()}, integer()}, 
         state_data()) ->
    {keep_state, state_data()}
  | {stop, normal, state_data()}
  | {next_state, s9, state_data(), [{next_event, internal, {ack}}]}
  | {next_state, s6, state_data(), [{next_event, internal, {fibonacci}}]}.
s5(internal, {error}, #state_data{a_pid = APid} = Data) ->
    case make_choice_error(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            io:format("B: s5 Sending error to a ~n", []),
            gen_b:send_s5_error(APid, Data),
            {stop, normal, Data}
    end;
s5(cast, {APid, {stop}}, #state_data{a_pid = APid} = Data) ->
    case make_choice_stop(Data) of
        1 ->
            {next_state, s9, Data, [{next_event, internal, {ack}}]};
        2 ->
            io:format("B: s5 Sending error to a ~n", []),
            gen_b:send_s5_error(APid, Data),
            {stop, normal, Data}
    end;
s5(cast, {APid, {fibonacci, Num}}, #state_data{a_pid = APid, curr_value = Curr} = Data) ->
    % Shift previous curr into prev and set new curr
    NewData = Data#state_data{prev_value = Curr, curr_value = Num},
    case make_choice_fibonacci(NewData) of
        1 ->
            {next_state, s6, NewData, [{next_event, internal, {fibonacci}}]};
        2 ->
            io:format("B: s5 Sending error to a ~n", []),
            gen_b:send_s5_error(APid, NewData),
            {stop, normal, NewData}
    end.

-spec s6(internal, {fibonacci}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {error}}]}.
s6(internal, {fibonacci}, #state_data{a_pid = APid, prev_value = Prev, curr_value = Curr} = Data) ->
    Next = Prev + Curr,
    NewData = Data#state_data{curr_value = Next},
    io:format("B: s6 Sending fibonacci ~p to a~n", [NewData#state_data.curr_value]),
    gen_b:send_s6_fibonacci(APid,NewData#state_data.curr_value, NewData),
    {next_state, s5, NewData, [{next_event, internal, {error}}]}.

-define(ERROR_INTERVAL, 5).
-spec make_choice_error(state_data()) -> integer().
make_choice_error(_Data) -> 1.

-define(MAX_STOP_ITER, 8).
-spec make_choice_stop(state_data()) -> integer().
make_choice_stop(#state_data{mc_counter_1 = Count}) ->
    if
        Count < ?MAX_STOP_ITER -> 1;  % next_state branch
        true -> 2                % stop branch
    end.

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {ack}, #state_data{a_pid = APid} = Data) ->
    io:format("B: s9 Sending ack to a ~n", []),
    gen_b:send_s9_ack(APid, Data),
    {stop, normal, Data}.
