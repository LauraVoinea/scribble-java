-module(b).
-behaviour(gen_b).

-export([init/1, callback_mode/0, start_link/0, make_choice_Timeout/1, s1/3, make_choice_a1/1, s2/3, make_choice_s3/1, s3/3]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {Timeout}}]}.
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
    CPid = case whereis(c) of
        undefined ->
            io:format("c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(c);
        Pid ->
            Pid
    end,
    CPid ! {b_pid, self()},
    Data = #state_data{mc_counter_1 = 0, a_pid = APid, c_pid = CPid},
    io:format("b initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {'Timeout'}}]}.

-spec s3(internal, {atom()}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()}.
s3(internal, {a5}, #state_data{a_pid = APid} = Data) ->
    gen_b:send_a5(APid, a5),
    {stop, normal, Data};
s3(internal, {'Timeout'}, #state_data{c_pid = CPid} = Data) ->
    gen_b:send_Timeout(CPid, 'Timeout'),
    {stop, normal, Data}.

-spec make_choice_Timeout(state_data()) -> integer().
make_choice_Timeout(_Data) ->
    rand:uniform(2).

-spec make_choice_s3(state_data()) -> integer().
make_choice_s3(_Data) ->
    rand:uniform(2).

-spec make_choice_a1(state_data()) -> integer().
make_choice_a1(_Data) ->
    rand:uniform(2).

-spec s1(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {next_state, s3, state_data()} | {next_state, s3, state_data(), [term()]} | {next_state, s2, state_data(), [{next_event, internal, {a2}}]}.
s1(internal, {'Timeout'}, #state_data{a_pid = APid} = Data) ->
    case make_choice_Timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            gen_b:send_Timeout(APid, Data),
            case make_choice_s3(Data) of
                a5 ->
                    {next_state, s3, Data, [{next_event, internal, {a5}}]};
                'Timeout' ->
                    {next_state, s3, Data, [{next_event, internal, {'Timeout'}}]}
            end
    end;
s1(cast, {APid, {a1}}, #state_data{a_pid = APid} = Data) ->
    case make_choice_a1(Data) of
        1 ->
            {next_state, s2, Data, [{next_event, internal, {a2}}]};
        2 ->
            gen_b:send_a1(APid, Data),
            {next_state, s2, Data, [{next_event, internal, {a2}}]}
    end.

-spec s2(internal, {atom()}, state_data()) -> {next_state, s3, state_data()} | {next_state, s3, state_data(), [term()]}.
s2(internal, {a2}, #state_data{c_pid = CPid} = Data) ->
    gen_b:send_a2(CPid, a2),
    case make_choice_s3(Data) of
        a5 ->
            {next_state, s3, Data, [{next_event, internal, {a5}}]};
        'Timeout' ->
            {next_state, s3, Data, [{next_event, internal, {'Timeout'}}]}
    end.

