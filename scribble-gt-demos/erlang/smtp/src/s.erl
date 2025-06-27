-module(s).
-behaviour(gen_s).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 make_choice_Timeout/1,
	 s5/3,
	 make_choice_Ehlo/1,
	 make_choice_Quit/1,
	 make_choice_s7/1,
	 s7/3,
	 s10/3,
	 s11/3,
	 s12/3,
	 make_choice_s14/1,
	 s14/3,
	 s18/3,
	 make_choice_s19/1,
	 s19/3,
	 s21/3,
	 make_choice_s22/1,
	 s22/3,
	 s26/3,
	 s27/3,
	 s32/3,
	 make_choice_Data/1,
	 s33/3,
	 s35/3,
	 s40/3,
	 s43/3,
	 s48/3,
	 s50/3,
	 s52/3
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
    Data = #state_data{mc_counter_1 = 0},
    io:format("s initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {'220'}}]}.

-spec make_choice_Data(state_data()) -> integer().
make_choice_Data(_Data) ->
    rand:uniform(2).

-spec s50(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s50(internal, {'Ack'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s50 Sending Ack to C ~n", []),
    gen_s:send_s50_Ack(CPid, Data),
    {stop, normal, Data}.

-spec make_choice_Ehlo(state_data()) -> integer().
make_choice_Ehlo(_Data) ->
    rand:uniform(2).

-spec s52(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s52(internal, {'Ack'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s52 Sending Ack to C ~n", []),
    gen_s:send_s52_Ack(CPid, Data),
    {stop, normal, Data}.

-spec s33(internal, {atom()}, state_data()) -> {next_state, s35, state_data()}.
s33(internal, {'354'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s33 Sending 354 to C ~n", []),
    gen_s:send_s33_354(CPid, Data),
    {next_state, s35, Data}.

-spec s11(internal, {atom()}, state_data()) -> {next_state, s12, state_data()}.
s11(internal, {'220'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s11 Sending 220 to C ~n", []),
    gen_s:send_s11_220(CPid, Data),
    {next_state, s12, Data}.

-spec s32(internal, {atom()}, state_data()) -> 
    {keep_state, state_data()} | 
    {stop, normal, state_data()} | 
    {next_state, s33, state_data(), [{next_event, internal, {'354'}}]}.
s32(internal, {'Timeout'}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_Timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            gen_s:send_s32_Timeout(CPid, Data),
            io:format("S: s32 Sending Timeout to C ~n", []),
            {stop, normal, Data}
    end;
s32(cast, {CPid, {'Data'}}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_Data(Data) of
        1 ->
            {next_state, s33, Data, [{next_event, internal, {'354'}}]};
        2 ->
            gen_s:send_s32_Timeout(CPid, Data),
            {stop, normal, Data}
    end.

s10(cast, {CPid, {'StartTls'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s11, Data, [{next_event, internal, {'220'}}]};
s10(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s50, Data, [{next_event, internal, {'Ack'}}]}.

-spec s35(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s35, state_data()} | 
    {next_state, s40, state_data(), [{next_event, internal, {'250'}}]}.
s35(cast, {CPid, {'Subject'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s35, Data};
s35(cast, {CPid, {'DataLine'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s35, Data};
s35(cast, {CPid, {'EndOfData'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s40, Data, [{next_event, internal, {'250'}}]}.

s12(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s48, Data, [{next_event, internal, {'Ack'}}]};
s12(cast, {CPid, {'Ehlo'}}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_s14(Data) of
        1 ->
            {next_state, s14, Data, [{next_event, internal, {'250d'}}]};
        2 ->
            {next_state, s14, Data, [{next_event, internal, {'250'}}]}
    end.

-spec make_choice_s7(state_data()) -> integer().
make_choice_s7(_Data) ->
    rand:uniform(2).

-spec s14(internal, {atom()}, state_data()) -> 
    {next_state, s14, state_data(), [{next_event, internal, {'250d'}}]} |
    {next_state, s14, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s18, state_data()}.
s14(internal, {'250d'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s14 Sending 250d to C ~n", []),
    gen_s:send_s14_250d(CPid, Data),
    case make_choice_s14(Data) of
        1 ->
            {next_state, s14, Data, [{next_event, internal, {'250d'}}]};
        2 ->
            {next_state, s14, Data, [{next_event, internal, {'250'}}]}
    end;
s14(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s14 Sending 250 to C ~n", []),
    gen_s:send_s14_250(CPid, Data),
    {next_state, s18, Data}.

-spec make_choice_s22(state_data()) -> integer().
make_choice_s22(_Data) ->
    rand:uniform(2).

-spec s19(internal, {atom()}, state_data()) -> {next_state, s18, state_data()} | {next_state, s21, state_data()}.
s19(internal, {'535'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s19 Sending 535 to C ~n", []),
    gen_s:send_s19_535(CPid, Data),
    {next_state, s18, Data};
s19(internal, {'235'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s19 Sending 235 to C ~n", []),
    gen_s:send_s19_235(CPid, Data),
    {next_state, s21, Data}.

-spec s18(cast, {pid(), {atom()}}, state_data()) -> 
    {stop, normal, state_data()} |
    {next_state, s19, state_data(), [{next_event, internal, {'235'}}]} |
    {next_state, s19, state_data(), [{next_event, internal, {'535'}}]}.
s18(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    {stop, normal, Data};
s18(cast, {CPid, {'Auth'}}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_s19(Data) of
        1 ->
            {next_state, s19, Data, [{next_event, internal, {'235'}}]};
        2 ->
            {next_state, s19, Data, [{next_event, internal, {'535'}}]}
    end.

-spec make_choice_Quit(state_data()) -> integer().
make_choice_Quit(_Data) ->
    rand:uniform(2).

-spec s1(internal, {atom()}, state_data()) -> 
    {next_state, s5, state_data(), [{next_event, internal, {'Timeout'}}]}.
s1(internal, {'220'}, Data) ->
    Data1 = connection(Data),
    CPid = Data1#state_data.c_pid,
    io:format("S: s1 Sending 220 to C ~n", []),
    gen_s:send_s1_220(CPid, Data),
    {next_state, s5, Data1, [{next_event, internal, {'Timeout'}}]}.

-spec s5(internal | cast, {atom()} | {pid(), {atom()}}, state_data()) -> 
    {keep_state, state_data()} |
    {next_state, s7, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s7, state_data(), [{next_event, internal, {'250d'}}]} |
    {next_state, s52, state_data(), [{next_event, internal, {'Ack'}}]} |
    {stop, normal, state_data()}.
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
    case make_choice_Ehlo(Data) of
        1 ->
            case make_choice_s7(Data) of
                1 ->
                    {next_state, s7, Data, [{next_event, internal, {'250'}}]};
                2 ->
                    {next_state, s7, Data, [{next_event, internal, {'250d'}}]}
            end;
        2 ->
            gen_s:send_s5_Timeout(CPid, Data),
            {stop, normal, Data}
    end;
s5(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_Quit(Data) of
        1 ->
            {next_state, s52, Data, [{next_event, internal, {'Ack'}}]};
        2 ->
            gen_s:send_s5_Timeout(CPid, Data),
            {stop, normal, Data}
    end.

-spec s7(internal, {atom()}, state_data()) -> 
    {next_state, s10, state_data()} |
    {next_state, s14, state_data(), [{next_event, internal, {'250d'}}]} |
    {next_state, s14, state_data(), [{next_event, internal, {'250'}}]}.
s7(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s7 Sending 250 to C ~n", []),
    gen_s:send_s7_250(CPid, Data),
    {next_state, s10, Data};
s7(internal, {'250d'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s7 Sending 250d to C ~n", []),
    gen_s:send_s7_250d(CPid, Data),
    case make_choice_s14(Data) of
        1 ->
            {next_state, s14, Data, [{next_event, internal, {'250d'}}]};
        2 ->
            {next_state, s14, Data, [{next_event, internal, {'250'}}]}
    end.

-spec make_choice_Timeout(state_data()) -> integer() | integer().
make_choice_Timeout(_Data) ->
    rand:uniform(2).

-spec s40(internal, {atom()}, state_data()) -> {next_state, s21, state_data()}.
s40(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s40 Sending 250 to C ~n", []),
    gen_s:send_s40_250(CPid, Data),
    {next_state, s21, Data}.

-spec make_choice_s19(state_data()) -> integer().
make_choice_s19(_Data) ->
    rand:uniform(2).

-spec s22(internal, {atom()}, state_data()) -> {next_state, s26, state_data()} | {next_state, s21, state_data()}.
s22(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s22 Sending 250 to C ~n", []),
    gen_s:send_s22_250(CPid, Data),
    {next_state, s26, Data};
s22(internal, {'501'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s22 Sending 501 to C ~n", []),
    gen_s:send_s22_501(CPid, Data),
    {next_state, s21, Data}.

-spec s43(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s43(internal, {'221'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s43 Sending 221 to C ~n", []),
    gen_s:send_s43_221(CPid, Data),
    {stop, normal, Data}.

-spec s21(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s22, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s22, state_data(), [{next_event, internal, {'501'}}]} |
    {next_state, s43, state_data(), [{next_event, internal, {'221'}}]}.
s21(cast, {CPid, {'Mail'}}, #state_data{c_pid = CPid} = Data) ->
    case make_choice_s22(Data) of
        1 ->
            {next_state, s22, Data, [{next_event, internal, {'250'}}]};
        2 ->
            {next_state, s22, Data, [{next_event, internal, {'501'}}]}
    end;
s21(cast, {CPid, {'Quit'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s43, Data, [{next_event, internal, {'221'}}]}.

-spec s48(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s48(internal, {'Ack'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s48 Sending Ack to C ~n", []),
    gen_s:send_s48_Ack(CPid, Data),
    {stop, normal, Data}.

-spec s26(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s27, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s32, state_data(), [{next_event, internal, {'Timeout'}}]}.
s26(cast, {CPid, {'Rcpt'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s27, Data, [{next_event, internal, {'250'}}]};
s26(cast, {CPid, {'Bogus'}}, #state_data{c_pid = CPid} = Data) ->
    {next_state, s32, Data, [{next_event, internal, {'Timeout'}}]}.

-spec make_choice_s14(state_data()) -> integer().
make_choice_s14(_Data) ->
    rand:uniform(2).

-spec s27(internal, {atom()}, state_data()) -> {next_state, s26, state_data()}.
s27(internal, {'250'}, #state_data{c_pid = CPid} = Data) ->
    io:format("S: s27 Sending 250 to C ~n", []),
    gen_s:send_s27_250(CPid, Data),
    {next_state, s26, Data}.

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

