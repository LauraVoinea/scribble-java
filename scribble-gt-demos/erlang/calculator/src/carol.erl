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
    Data = #state_data{mc_counter_1 = 0},
    io:format("carol initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {first}}]}.

-spec connect(state_data()) -> state_data().
connect(Data) ->
    SrvPid = case whereis(srv) of
        undefined ->
            io:format("srv is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(srv);
        Pid ->
            Pid
    end,

    AlicePid = case whereis(alice) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(alice);
        Pid1 ->
            Pid1
    end,
    Data#state_data{srv_pid = SrvPid, alice_pid = AlicePid}.

-spec s3(internal, {atom()}, state_data()) ->
    {next_state, s7, state_data(), [{next_event, internal, {sum}}]} |
    {next_state, s7, state_data(), [{next_event, internal, {diff}}]}.
s3(internal, {second}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("Carol: s3 Sending second to Srv ~n", []),

    Number = 2,

    gen_carol:send_s3_second(SrvPid, Number, Data),
    case make_choice_s7(Data) of
        1 ->
            {next_state, s7, Data, [{next_event, internal, {sum}}]};
        2 ->
            {next_state, s7, Data, [{next_event, internal, {diff}}]}
    end.

-spec s5(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s5(internal, {cancel}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("Carol: s5 Sending cancel to Alice ~n", []),
    gen_carol:send_s5_cancel(AlicePid, Data),
    {stop, normal, Data}.

-spec s11(cast, {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | 
    {next_state, s12, state_data(), [{next_event, internal, {diff_result}}]}.
s11(cast, {SrvPid, {timeout}}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("Carol: s11 Received timeout from Srv ~n", []),
    {next_state, s5, Data, [{next_event, internal, {cancel}}]};
s11(cast, {SrvPid, {result_diff, Result}}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("Carol: s11 Received result_diff ~p from Srv ~n", [Result]),
    {next_state, s12, Data, [{next_event, internal, {diff_result}}]}.

-spec s7(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s8, state_data()} |
    {next_state, s11, state_data()} |
    {next_state, s5, state_data(), [{next_event, internal, {cancel}}]}.
s7(internal, {sum}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("Carol: s7 Sending sum to Srv ~n", []),
    gen_carol:send_s7_sum(SrvPid, Data),
    {next_state, s8, Data};
s7(internal, {diff}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("Carol: s7 Sending diff to Srv ~n", []),
    gen_carol:send_s7_diff(SrvPid, Data),
    {next_state, s11, Data};
s7(cast, {SrvPid, {timeout}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {cancel}}]}.

-spec s12(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(internal, {diff_result}, #state_data{alice_pid = AlicePid} = Data) ->
    Result = 24,
    io:format("Carol: s12 Sending diff_result ~p to Alice ~n", [Result]),
    gen_carol:send_s12_diff_result(AlicePid, Result, Data),
    {stop, normal, Data}.

-spec s8(cast, {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | 
    {next_state, s9, state_data(), [{next_event, internal, {sum_result}}]}.
s8(cast, {SrvPid, {timeout}}, #state_data{srv_pid = SrvPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {cancel}}]};
s8(cast, {SrvPid, {result_sum, Result}}, #state_data{srv_pid = SrvPid} = Data) ->
    io:format("Carol: s8 Received result_sum ~p from Srv ~n", [Result]),
    {next_state, s9, Data, [{next_event, internal, {sum_result}}]}.

-spec make_choice_s7(state_data()) -> integer().
make_choice_s7(_Data) ->
    rand:uniform(2).

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {sum_result}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("Carol: s9 Sending sum_result to Alice ~n", []),
    Result = 42,
    gen_carol:send_s9_sum_result(AlicePid,Result, Data),
    {stop, normal, Data}.

-spec s1(internal, {atom()}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {second}}]}.
s1(internal, {first}, Data) ->
    Data1 = connect(Data),
    SrvPid = Data1#state_data.srv_pid,
    io:format("Carol: s1 Sending first to Srv ~n", []),
    Number = 1, % Example number, can be replaced with actual logic
    gen_carol:send_s1_first(SrvPid, Number, Data),
    {next_state, s3, Data1, [{next_event, internal, {second}}]}.

