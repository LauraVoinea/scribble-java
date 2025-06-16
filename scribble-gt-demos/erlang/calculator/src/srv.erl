-module(srv).
-behaviour(gen_srv).

-export([init/1, callback_mode/0, start_link/0, s14/3, s16/3, make_choice_timeout/1, s19/3, make_choice_diff/1, make_choice_sum/1, s20/3, s22/3]).

-include("srv.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), carol_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_srv:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s14, state_data()}.
init([]) ->
    CarolPid = case whereis(carol) of
        undefined ->
            io:format("carol is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(carol);
        Pid ->
            Pid
    end,
    CarolPid ! {srv_pid, self()},
    AlicePid = case whereis(alice) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(alice);
        Pid ->
            Pid
    end,
    AlicePid ! {srv_pid, self()},
    Data = #state_data{mc_counter_1 = 0, carol_pid = CarolPid, alice_pid = AlicePid},
    io:format("srv initialized ~n", []),
    {ok, s14, Data}.

-spec s20(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s20(internal, {result_sum}, #state_data{carol_pid = CarolPid} = Data) ->
    io:format("B: s20 Sending result_sum to Carol ~n", []),
    gen_srv:send_s20_result_sum(CarolPid, Data),
    {stop, normal, Data}.

-spec make_choice_sum(state_data()) -> integer().
make_choice_sum(_Data) ->
    rand:uniform(2).

-spec make_choice_timeout(state_data()) -> integer().
make_choice_timeout(_Data) ->
    rand:uniform(2).

-spec s22(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s22(internal, {result_diff}, #state_data{carol_pid = CarolPid} = Data) ->
    io:format("B: s22 Sending result_diff to Carol ~n", []),
    gen_srv:send_s22_result_diff(CarolPid, Data),
    {stop, normal, Data}.

-spec s14(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s16, state_data()}.
s14(cast, {CarolPid, {first, number}}, #state_data{carol_pid = CarolPid} = Data) ->
    {next_state, s16, Data}.

-spec make_choice_diff(state_data()) -> integer().
make_choice_diff(_Data) ->
    rand:uniform(2).

-spec s16(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s19, state_data(), [{next_event, internal, {timeout}}]}.
s16(cast, {CarolPid, {second, number}}, #state_data{carol_pid = CarolPid} = Data) ->
    {next_state, s19, Data, [{next_event, internal, {timeout}}]}.

-spec s19(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {next_state, s22, state_data(), [{next_event, internal, {result_diff}}]} | {next_state, s20, state_data(), [{next_event, internal, {result_sum}}]}.
s19(internal, {timeout}, #state_data{carol_pid = CarolPid} = Data) ->
    case make_choice_timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            io:format("B: s19 Sending timeout to Carol ~n", []),
            gen_srv:send_s19_timeout(CarolPid, Data),
            {stop, normal, Data}
    end;
s19(cast, {CarolPid, {diff}}, #state_data{carol_pid = CarolPid} = Data) ->
    case make_choice_diff(Data) of
        1 ->
            {next_state, s22, Data, [{next_event, internal, {result_diff}}]};
        2 ->
            io:format("B: s19 Sending timeout to Carol ~n", []),
            gen_srv:send_s19_timeout(CarolPid, Data),
            {stop, normal, Data}
    end;
s19(cast, {CarolPid, {sum}}, #state_data{carol_pid = CarolPid} = Data) ->
    case make_choice_sum(Data) of
        1 ->
            {next_state, s20, Data, [{next_event, internal, {result_sum}}]};
        2 ->
            io:format("B: s19 Sending timeout to Carol ~n", []),
            gen_srv:send_s19_timeout(CarolPid, Data),
            {stop, normal, Data}
    end.

