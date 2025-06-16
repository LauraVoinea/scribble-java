-module(carol).
-behaviour(gen_carol).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, make_choice_s7/1, s7/3, s8/3, s9/3, s11/3, s12/3, s5/3]).

-include("carol.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), srv_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_carol:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {first}}]}.
init([]) ->
    SrvPid = case whereis(srv) of
        undefined ->
            io:format("srv is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(srv);
        Pid ->
            Pid
    end,
    SrvPid ! {carol_pid, self()},
    AlicePid = case whereis(alice) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(alice);
        Pid ->
            Pid
    end,
    AlicePid ! {carol_pid, self()},
    Data = #state_data{mc_counter_1 = 0, srv_pid = SrvPid, alice_pid = AlicePid},
    io:format("carol initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {first}}]}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s7, state_data()} | {next_state, s7, state_data(), [term()]}.
s3(internal, {second}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("B: s3 Sending second to Srv ~n", []),
    gen_carol:send_s3_second(SrvPid, Data),
    case make_choice_s7(Data) of
        sum ->
            {next_state, s7, Data, [{next_event, internal, {sum}}]};
        diff ->
            {next_state, s7, Data, [{next_event, internal, {diff}}]}
    end.

-spec s5(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s5(internal, {cancel}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("B: s5 Sending cancel to Alice ~n", []),
    gen_carol:send_s5_cancel(AlicePid, Data),
    {stop, normal, Data}.

-spec s11(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {next_state, s12, state_data(), [{next_event, internal, {diff_result}}]}.
s11(cast, {SrvPid, {timeout}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {cancel}}]};
s11(cast, {SrvPid, {result_diff, result}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s12, Data, [{next_event, internal, {diff_result}}]}.

-spec s7(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s8, state_data()} | {next_state, s11, state_data()} | {next_state, s5, state_data(), [{next_event, internal, {cancel}}]}.
s7(internal, {sum}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("B: s7 Sending sum to Srv ~n", []),
    gen_carol:send_s7_sum(SrvPid, Data),
    {next_state, s8, Data};
s7(internal, {diff}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("B: s7 Sending diff to Srv ~n", []),
    gen_carol:send_s7_diff(SrvPid, Data),
    {next_state, s11, Data};
s7(cast, {SrvPid, {timeout}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {cancel}}]}.

-spec s12(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(internal, {diff_result}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("B: s12 Sending diff_result to Alice ~n", []),
    gen_carol:send_s12_diff_result(AlicePid, Data),
    {stop, normal, Data}.

-spec s8(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {next_state, s9, state_data(), [{next_event, internal, {sum_result}}]}.
s8(cast, {SrvPid, {timeout}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {cancel}}]};
s8(cast, {SrvPid, {result_sum, Result}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s9, Data, [{next_event, internal, {sum_result}}]}.

-spec make_choice_s7(state_data()) -> integer().
make_choice_s7(_Data) ->
    rand:uniform(2).

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {sum_result}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("B: s9 Sending sum_result to Alice ~n", []),
    gen_carol:send_s9_sum_result(AlicePid, Data),
    {stop, normal, Data}.

-spec s1(internal, {atom()}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {second}}]}.
s1(internal, {first}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("B: s1 Sending first to Srv ~n", []),
    gen_carol:send_s1_first(SrvPid, Data, 3),
    {next_state, s3, Data, [{next_event, internal, {second}}]}.

