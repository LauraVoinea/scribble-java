-module(b).
-behaviour(gen_b).

-export([init/1, callback_mode/0, start_link/0, make_choice_Timeout/1, s12/3, make_choice_a1/1, s13/3, s14/3, s10/3]).

-include("b.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), a_pid :: pid() | undefined, c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_b:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s12, state_data(), [{next_event, internal, {Timeout}}]}.
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
    {ok, s12, Data, [{next_event, internal, {'Timeout'}}]}.

-spec s10(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s10(internal, {'Timeout'}, #state_data{c_pid = CPid} = Data) ->
    gen_b:send_Timeout(CPid, 'Timeout'),
    {stop, normal, Data}.

-spec s13(internal, {atom()}, state_data()) -> {next_state, s14, state_data(), [{next_event, internal, {a5}}]}.
s13(internal, {a2}, #state_data{c_pid = CPid} = Data) ->
    gen_b:send_a2(CPid, a2),
    {next_state, s14, Data, [{next_event, internal, {a5}}]}.

-spec s12(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {next_state, s10, state_data(), [{next_event, internal, {Timeout}}]} | {next_state, s13, state_data(), [{next_event, internal, {a2}}]}.
s12(internal, {'Timeout'}, #state_data{a_pid = APid} = Data) ->
    case make_choice_Timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            gen_b:send_Timeout(APid, Data),
            {next_state, s10, Data, [{next_event, internal, {'Timeout'}}]}
    end;
s12(cast, {APid, {a1}}, #state_data{a_pid = APid} = Data) ->
    case make_choice_a1(Data) of
        1 ->
            {next_state, s13, Data, [{next_event, internal, {a2}}]};
        2 ->
            gen_b:send_a1(APid, Data),
            {next_state, s13, Data, [{next_event, internal, {a2}}]}
    end.

-spec s14(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s14(internal, {a5}, #state_data{a_pid = APid} = Data) ->
    gen_b:send_a5(APid, a5),
    {stop, normal, Data}.

-spec make_choice_Timeout(state_data()) -> integer().
make_choice_Timeout(_Data) ->
    rand:uniform(2).

-spec make_choice_a1(state_data()) -> integer().
make_choice_a1(_Data) ->
    rand:uniform(2).

