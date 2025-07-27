-module(s).
-behaviour(gen_s).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 make_choice_Timeout/1,
	 s5/3,
	 make_choice_Ehlo/1,
	 make_choice_QuitCommit/1,
	 s6/3,
	 make_choice_s8/1,
	 s8/3,
	 s11/3,
	 s12/3,
	 s13/3,
	 make_choice_s15/1,
	 s15/3,
	 s19/3,
	 make_choice_s20/1,
	 s20/3,
	 s22/3,
	 make_choice_s23/1,
	 s23/3,
	 s27/3,
	 s28/3,
	 s33/3,
	 make_choice_Data/1,
	 s34/3,
	 s36/3,
	 s41/3,
	 s44/3,
	 s49/3,
	 s51/3,
	 s53/3
	]).

-include("s.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), c_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_s:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {'220'}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0, mc_counter_2 = 0},
    io:format("s initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {'220'}}]}.

-spec make_choice_Data(state_data()) -> integer().
make_choice_Data(_Data) ->
    rand:uniform(2).

-spec s51(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s51(internal, {'Ack'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s51 Sending Ack to C ~n", []),
    gen_s:send_s51_Ack(CPid, Data),
    {stop, normal, Data}.

-spec make_choice_Ehlo(state_data()) -> integer().
make_choice_Ehlo(_Data) ->
    rand:uniform(2).

-spec s53(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s53(internal, {'AckCommit'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s53 Sending AckCommit to C ~n", []),
    gen_s:send_s53_AckCommit(CPid, Data),
    {stop, normal, Data}.

-spec s33(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s34, state_data(), [{next_event, internal, {'354'}}]} |
    {keep_state, state_data()}.
s33(internal, {'Timeout'}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_Timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            gen_s:send_s33_Timeout(CPid, Data),
            io:format("S: s33 Sending Timeout to C ~n", []),
            {stop, normal, Data}
    end;
s33(cast, {CPid, {'Data'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s33 Received Data  from C ~p ~n", [CPid]),
    case make_choice_Data(Data) of
        1 ->
            {next_state, s34, Data, [{next_event, internal, {'354'}}]};
        2 ->
            gen_s:send_s33_Timeout(CPid, Data),
            {stop, normal, Data}
    end.

-spec s11(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s51, state_data(), [{next_event, internal, {'Ack'}}]} |
    {keep_state, state_data()} |
    {next_state, s12, state_data(), [{next_event, internal, {'220'}}]}.
s11(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s11 Received Quit  from C ~p ~n", [CPid]),
    {next_state, s51, Data, [{next_event, internal, {'Ack'}}]};
s11(cast, {CPid, {'StartTls'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s11 Received StartTls  from C ~p ~n", [CPid]),
    {next_state, s12, Data, [{next_event, internal, {'220'}}]}.

-spec s13(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s49, state_data(), [{next_event, internal, {'Ack'}}]} |
    {keep_state, state_data()} |
    {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]}.
s13(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s13 Received Quit  from C ~p ~n", [CPid]),
    {next_state, s49, Data, [{next_event, internal, {'Ack'}}]};
s13(cast, {CPid, {'Ehlo1'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s13 Received Ehlo1  from C ~p ~n", [CPid]),
    case make_choice_s15(Data) of
        1 ->
            {next_state, s15, Data, [{next_event, internal, {'250'}}]};
        2 ->
            {next_state, s15, Data, [{next_event, internal, {'250d'}}]}
    end.

-spec s34(internal, {atom()}, state_data()) -> {next_state, s36, state_data()}.
s34(internal, {'354'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s34 Sending 354 to C ~n", []),
    gen_s:send_s34_354(CPid, Data),
    {next_state, s36, Data}.

-spec s12(internal, {atom()}, state_data()) -> {next_state, s13, state_data()}.
s12(internal, {'220'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s12 Sending 220 to C ~n", []),
    gen_s:send_s12_220(CPid, Data),
    {next_state, s13, Data}.

-spec s15(internal, {atom()}, state_data()) ->
    {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]} |
    {keep_state, state_data()} |
    {next_state, s19, state_data()}.
s15(internal, {'250d'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s15 Sending 250d to C ~n", []),
    gen_s:send_s15_250d(CPid, Data),
    case make_choice_s15(Data) of
        1 ->
            {next_state, s15, Data, [{next_event, internal, {'250'}}]};
        2 ->
            {next_state, s15, Data, [{next_event, internal, {'250d'}}]}
    end;
s15(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s15 Sending 250 to C ~n", []),
    gen_s:send_s15_250(CPid, Data),
    {next_state, s19, Data}.

-spec make_choice_s8(state_data()) -> integer().
make_choice_s8(_Data) ->
    rand:uniform(2).

-spec s36(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s36, state_data()} |
    {next_state, s41, state_data(), [{next_event, internal, {'250'}}]} |
    {keep_state, state_data()}.
s36(cast, {CPid, {'Subject'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s36 Received Subject  from C ~p ~n", [CPid]),
    {next_state, s36, Data};
s36(cast, {CPid, {'DataLine'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s36 Received DataLine  from C ~p ~n", [CPid]),
    {next_state, s36, Data};
s36(cast, {CPid, {'EndOfData'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s36 Received EndOfData  from C ~p ~n", [CPid]),
    {next_state, s41, Data, [{next_event, internal, {'250'}}]}.

-spec make_choice_s23(state_data()) -> integer().
make_choice_s23(_Data) ->
    rand:uniform(2).

-spec make_choice_s20(state_data()) -> integer().
make_choice_s20(_Data) ->
    rand:uniform(2).

-spec s19(cast, {pid(), {atom(), term()}}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s20, state_data(), [{next_event, internal, {'535'}}]} |
    {next_state, s20, state_data(), [{next_event, internal, {'235'}}]} |
    {keep_state, state_data()}.
s19(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s19 Received Quit  from C ~p ~n", [CPid]),
    {stop, normal, Data};
s19(cast, {CPid, {'Auth'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s19 Received Auth  from C ~p ~n", [CPid]),
    case make_choice_s20(Data) of
        1 ->
            {next_state, s20, Data, [{next_event, internal, {'535'}}]};
        2 ->
            {next_state, s20, Data, [{next_event, internal, {'235'}}]}
    end.

-spec s1(internal, {atom()}, state_data()) ->
    {next_state, s5, state_data(), [{next_event, internal, {'Timeout'}}]} |
    {keep_state, state_data()}.
s1(internal, {'220'}, Data) ->
    NewData = connection(Data),
    CPid = NewData#state_data.c_pid,
    io:format("S: s1 Sending 220 to C ~n", []),
    gen_s:send_s1_220(CPid, Data),
    {next_state, s5, NewData, [{next_event, internal, {'Timeout'}}]}.

-spec s5(internal | EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s6, state_data(), [{next_event, internal, {'EhloCommit'}}]} |
    {keep_state, state_data()} |
    {next_state, s53, state_data(), [{next_event, internal, {'AckCommit'}}]}.
s5(internal, {'Timeout'}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_Timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            gen_s:send_s5_Timeout(CPid, Data),
            io:format("S: s5 Sending Timeout to C ~n", []),
            {stop, normal, Data}
    end;
s5(cast, {CPid, {'Ehlo'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s5 Received Ehlo  from C ~p ~n", [CPid]),
    case make_choice_Ehlo(Data) of
        1 ->
            {next_state, s6, Data, [{next_event, internal, {'EhloCommit'}}]};
        2 ->
            gen_s:send_s5_Timeout(CPid, Data),
            {stop, normal, Data}
    end;
s5(cast, {CPid, {'QuitCommit'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s5 Received QuitCommit  from C ~p ~n", [CPid]),
    case make_choice_QuitCommit(Data) of
        1 ->
            {next_state, s53, Data, [{next_event, internal, {'AckCommit'}}]};
        2 ->
            gen_s:send_s5_Timeout(CPid, Data),
            {stop, normal, Data}
    end.

-spec s6(internal, {atom()}, state_data()) ->
    {next_state, s8, state_data(), [{next_event, internal, {'250d'}}]} |
    {next_state, s8, state_data(), [{next_event, internal, {'250'}}]} |
    {keep_state, state_data()}.
s6(internal, {'EhloCommit'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s6 Sending EhloCommit to C ~n", []),
    gen_s:send_s6_EhloCommit(CPid, Data),
    case make_choice_s8(Data) of
        1 ->
            {next_state, s8, Data, [{next_event, internal, {'250d'}}]};
        2 ->
            {next_state, s8, Data, [{next_event, internal, {'250'}}]}
    end.

-spec s8(internal, {atom()}, state_data()) ->
    {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]} |
    {keep_state, state_data()} |
    {next_state, s11, state_data()}.
s8(internal, {'250d'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s8 Sending 250d to C ~n", []),
    gen_s:send_s8_250d(CPid, Data),
    case make_choice_s15(Data) of
        1 ->
            {next_state, s15, Data, [{next_event, internal, {'250'}}]};
        2 ->
            {next_state, s15, Data, [{next_event, internal, {'250d'}}]}
    end;
s8(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s8 Sending 250 to C ~n", []),
    gen_s:send_s8_250(CPid, Data),
    {next_state, s11, Data}.

-spec make_choice_Timeout(state_data()) -> integer().
make_choice_Timeout(_Data) ->
    rand:uniform(2).

-spec s20(internal, {atom()}, state_data()) ->
    {next_state, s19, state_data()} |
    {next_state, s22, state_data()}.
s20(internal, {'535'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s20 Sending 535 to C ~n", []),
    gen_s:send_s20_535(CPid, Data),
    {next_state, s19, Data};
s20(internal, {'235'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s20 Sending 235 to C ~n", []),
    gen_s:send_s20_235(CPid, Data),
    {next_state, s22, Data}.

-spec s41(internal, {atom()}, state_data()) -> {next_state, s22, state_data()}.
s41(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s41 Sending 250 to C ~n", []),
    gen_s:send_s41_250(CPid, Data),
    {next_state, s22, Data}.

-spec s44(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s44(internal, {'221'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s44 Sending 221 to C ~n", []),
    gen_s:send_s44_221(CPid, Data),
    {stop, normal, Data}.

-spec s22(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s23, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s23, state_data(), [{next_event, internal, {'501'}}]} |
    {keep_state, state_data()} |
    {next_state, s44, state_data(), [{next_event, internal, {'221'}}]}.
s22(cast, {CPid, {'Mail'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s22 Received Mail  from C ~p ~n", [CPid]),
    case make_choice_s23(Data) of
        1 ->
            {next_state, s23, Data, [{next_event, internal, {'250'}}]};
        2 ->
            {next_state, s23, Data, [{next_event, internal, {'501'}}]}
    end;
s22(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s22 Received Quit  from C ~p ~n", [CPid]),
    {next_state, s44, Data, [{next_event, internal, {'221'}}]}.

-spec make_choice_s15(state_data()) -> integer().
make_choice_s15(_Data) ->
    rand:uniform(2).

-spec s23(internal, {atom()}, state_data()) ->
    {next_state, s27, state_data()} |
    {next_state, s22, state_data()}.
s23(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s23 Sending 250 to C ~n", []),
    gen_s:send_s23_250(CPid, Data),
    {next_state, s27, Data};
s23(internal, {'501'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s23 Sending 501 to C ~n", []),
    gen_s:send_s23_501(CPid, Data),
    {next_state, s22, Data}.

-spec s28(internal, {atom()}, state_data()) -> {next_state, s27, state_data()}.
s28(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s28 Sending 250 to C ~n", []),
    gen_s:send_s28_250(CPid, Data),
    {next_state, s27, Data}.

-spec s49(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s49(internal, {'Ack'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s49 Sending Ack to C ~n", []),
    gen_s:send_s49_Ack(CPid, Data),
    {stop, normal, Data}.

-spec s27(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s28, state_data(), [{next_event, internal, {'250'}}]} |
    {keep_state, state_data()} |
    {next_state, s33, state_data(), [{next_event, internal, {'Timeout'}}]}.
s27(cast, {CPid, {'Rcpt'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s27 Received Rcpt  from C ~p ~n", [CPid]),
    {next_state, s28, Data, [{next_event, internal, {'250'}}]};
s27(cast, {CPid, {'Bogus'}}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s27 Received Bogus  from C ~p ~n", [CPid]),
    {next_state, s33, Data, [{next_event, internal, {'Timeout'}}]}.

-spec make_choice_QuitCommit(state_data()) -> integer().
make_choice_QuitCommit(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("s connected ~n", []),
    CPid = case whereis(client) of
        undefined ->
            io:format("c is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(client);
        Pid_c ->
            Pid_c
    end,
    Data#state_data{c_pid = CPid}.

