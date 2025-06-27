-module(c).
-behaviour(gen_c).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 make_choice_s5/1,
	 s5/3,
	 s7/3,
	 make_choice_s10/1,
	 s10/3,
	 s11/3,
	 make_choice_s12/1,
	 s12/3,
	 s14/3,
	 make_choice_s18/1,
	 s18/3,
	 s19/3,
	 make_choice_s21/1,
	 s21/3,
	 s22/3,
	 make_choice_s26/1,
	 s26/3,
	 s27/3,
	 s32/3,
	 s33/3,
	 make_choice_s35/1,
	 s35/3,
	 s40/3,
	 s43/3,
	 s48/3,
	 s50/3,
	 s52/3
	]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), s_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_c:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("c initialized ~n", []),
    {ok, s1, Data}.

-spec make_choice_s5(state_data()) -> integer().
make_choice_s5(_Data) ->
    rand:uniform(2).

-spec s50(cast, {pid(), {atom()}}, state_data()) -> {stop, normal, state_data()}.
s50(cast, {SPid, {'Ack'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s52(cast, {pid(), {atom()}}, state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()}.
s52(cast, {SPid, {'Ack'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data};
s52(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s33(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s35, state_data(), [{next_event, internal, {'DataLine'}}]} |
    {next_state, s35, state_data(), [{next_event, internal, {'Subject'}}]} |
    {next_state, s35, state_data(), [{next_event, internal, {'EndOfData'}}]} |
    {stop, normal, state_data()}.
s33(cast, {SPid, {'354'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s35(Data) of
        1 ->
            {next_state, s35, Data, [{next_event, internal, {'DataLine'}}]};
        2 ->
            {next_state, s35, Data, [{next_event, internal, {'Subject'}}]};
        3 ->
            {next_state, s35, Data, [{next_event, internal, {'EndOfData'}}]}
    end;
s33(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s11(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s12, state_data(), [{next_event, internal, {'Quit'}}]} |
    {next_state, s12, state_data(), [{next_event, internal, {'Ehlo'}}]}.
s11(cast, {SPid, {'220'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s12(Data) of
        1 ->
            {next_state, s12, Data, [{next_event, internal, {'Quit'}}]};
        2 ->
            {next_state, s12, Data, [{next_event, internal, {'Ehlo'}}]}
    end.

-spec s32(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s33, state_data()} | {stop, normal, state_data()}.
s32(internal, {'Data'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s32 Sending Data to S ~n", []),
    gen_c:send_s32_Data(SPid, Data),
    {next_state, s33, Data};
s32(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s10(internal, {atom()}, state_data()) -> {next_state, s11, state_data()} | {next_state, s50, state_data()}.
s10(internal, {'StartTls'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s10 Sending StartTls to S ~n", []),
    gen_c:send_s10_StartTls(SPid, Data),
    {next_state, s11, Data};
s10(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s10 Sending Quit to S ~n", []),
    gen_c:send_s10_Quit(SPid, Data),
    {next_state, s50, Data}.

-spec s35(internal, {atom()}, state_data()) -> 
    {next_state, s40, state_data()} |
    {next_state, s35, state_data(), [{next_event, internal, {'DataLine'}}]} |
    {next_state, s35, state_data(), [{next_event, internal, {'Subject'}}]} |
    {next_state, s35, state_data(), [{next_event, internal, {'EndOfData'}}]}.
s35(internal, {'EndOfData'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s35 Sending EndOfData to S ~n", []),
    gen_c:send_s35_EndOfData(SPid, Data),
    {next_state, s40, Data};
s35(internal, {'DataLine'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s35 Sending DataLine to S ~n", []),
    gen_c:send_s35_DataLine(SPid, Data),
    case make_choice_s35(Data) of
        1 ->
            {next_state, s35, Data, [{next_event, internal, {'DataLine'}}]};
        2 ->
            {next_state, s35, Data, [{next_event, internal, {'Subject'}}]};
        3 ->
            {next_state, s35, Data, [{next_event, internal, {'EndOfData'}}]}
    end;
s35(internal, {'Subject'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s35 Sending Subject to S ~n", []),
    gen_c:send_s35_Subject(SPid, Data),
    case make_choice_s35(Data) of
        1 ->
            {next_state, s35, Data, [{next_event, internal, {'DataLine'}}]};
        2 ->
            {next_state, s35, Data, [{next_event, internal, {'Subject'}}]};
        3 ->
            {next_state, s35, Data, [{next_event, internal, {'EndOfData'}}]}
    end.

-spec make_choice_s26(state_data()) -> integer().
make_choice_s26(_Data) ->
    rand:uniform(2).

-spec s12(internal, {atom()}, state_data()) -> {next_state, s48, state_data()} | {next_state, s14, state_data()}.
s12(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s12 Sending Quit to S ~n", []),
    gen_c:send_s12_Quit(SPid, Data),
    {next_state, s48, Data};
s12(internal, {'Ehlo'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s12 Sending Ehlo to S ~n", []),
    gen_c:send_s12_Ehlo(SPid, Data),
    {next_state, s14, Data}.

-spec s14(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s18, state_data(), [{next_event, internal, {'Auth'}}]}|
    {next_state, s18, state_data(), [{next_event, internal, {'Quit'}}]} |
    {next_state, s14, state_data()}.
s14(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s18(Data) of
        1 ->
            {next_state, s18, Data, [{next_event, internal, {'Auth'}}]};
        2 ->
            {next_state, s18, Data, [{next_event, internal, {'Quit'}}]}
    end;
s14(cast, {SPid, {'250d'}}, #state_data{s_pid = SPid} = Data) ->
    {next_state, s14, Data}.

-spec s19(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s21, state_data(), [{next_event, internal, {'Quit'}}]} |
    {next_state, s21, state_data(), [{next_event, internal, {'Mail'}}]} |
    {next_state, s18, state_data(), [{next_event, internal, {'Auth'}}]} |
    {next_state, s18, state_data(), [{next_event, internal, {'Quit'}}]}.
s19(cast, {SPid, {'235'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s21(Data) of
        1 ->
            {next_state, s21, Data, [{next_event, internal, {'Quit'}}]};
        2 ->
            {next_state, s21, Data, [{next_event, internal, {'Mail'}}]}
    end;
s19(cast, {SPid, {'535'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s18(Data) of
        1 ->
            {next_state, s18, Data, [{next_event, internal, {'Auth'}}]};
        2 ->
            {next_state, s18, Data, [{next_event, internal, {'Quit'}}]}
    end.

-spec make_choice_s21(state_data()) -> integer().
make_choice_s21(_Data) ->
    rand:uniform(2).

-spec s18(internal, {atom()}, state_data()) -> {stop, normal, state_data()} | {next_state, s19, state_data()}.
s18(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s18 Sending Quit to S ~n", []),
    gen_c:send_s18_Quit(SPid, Data),
    {stop, normal, Data};
s18(internal, {'Auth'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s18 Sending Auth to S ~n", []),
    gen_c:send_s18_Auth(SPid, Data),
    {next_state, s19, Data}.

s1(cast, {SPid, {'220'}}, Data) ->
    Data1 = connection(Data),
    io:format("C: s1 Connected to S ~p ~n", [SPid]),
    case make_choice_s5(Data1) of
        1 ->
            io:format("C: s1 Choosing to Quit ~n", []),
            {next_state, s5, Data1, [{next_event, internal, {'Quit'}}]};
        2 ->
            io:format("C: s1 Choosing to Ehlo ~n", []),
            {next_state, s5, Data1, [{next_event, internal, {'Ehlo'}}]}
    end.

-spec s5(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s52, state_data()} | 
    {next_state, s7, state_data()} | 
    {stop, normal, state_data()}.
s5(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s5 Sending Quit to S ~n", []),
    gen_c:send_s5_Quit(SPid, Data),
    {next_state, s52, Data};
s5(internal, {'Ehlo'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s5 Sending Ehlo to S ~n", []),
    gen_c:send_s5_Ehlo(SPid, Data),
    {next_state, s7, Data};
s5(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s7(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s14, state_data()} | 
    {stop, normal, state_data()} |
    {next_state, s10, state_data(), [{next_event, internal, {'StartTls'}}]} |
    {next_state, s10, state_data(), [{next_event, internal, {'Quit'}}]}.
s7(cast, {SPid, {'250d'}}, #state_data{s_pid = SPid} = Data) ->
    {next_state, s14, Data};
s7(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data};
s7(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s10(Data) of
        1 ->
            {next_state, s10, Data, [{next_event, internal, {'StartTls'}}]};
        2 ->
            {next_state, s10, Data, [{next_event, internal, {'Quit'}}]}
    end.

s40(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s21(Data) of
        1 ->
            {next_state, s21, Data, [{next_event, internal, {'Quit'}}]};
        2 ->
            {next_state, s21, Data, [{next_event, internal, {'Mail'}}]}
    end.

s22(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s26(Data) of
        1 ->
            {next_state, s26, Data, [{next_event, internal, {'Bogus'}}]};
        2 ->
            {next_state, s26, Data, [{next_event, internal, {'Rcpt'}}]}
    end;
s22(cast, {SPid, {'501'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s21(Data) of
        1 ->
            {next_state, s21, Data, [{next_event, internal, {'Quit'}}]};
        2 ->
            {next_state, s21, Data, [{next_event, internal, {'Mail'}}]}
    end.

-spec s43(cast, {pid(), {atom()}}, state_data()) -> {stop, normal, state_data()}.
s43(cast, {SPid, {'221'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec s21(internal, {atom()}, state_data()) -> {next_state, s22, state_data()} | {next_state, s43, state_data()}.
s21(internal, {'Mail'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s21 Sending Mail to S ~n", []),
    gen_c:send_s21_Mail(SPid, Data),
    {next_state, s22, Data};
s21(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s21 Sending Quit to S ~n", []),
    gen_c:send_s21_Quit(SPid, Data),
    {next_state, s43, Data}.

-spec make_choice_s18(state_data()) -> integer().
make_choice_s18(_Data) ->
    rand:uniform(2).

-spec s48(cast, {pid(), {atom()}}, state_data()) -> {stop, normal, state_data()}.
s48(cast, {SPid, {'Ack'}}, #state_data{s_pid = SPid} = Data) ->
    {stop, normal, Data}.

-spec make_choice_s35(state_data()) -> integer().
make_choice_s35(_Data) ->
    rand:uniform(3).

-spec s26(internal, {atom()}, state_data()) -> 
    {next_state, s27, state_data()} |
    {next_state, s32, state_data(), [{next_event, internal, {'Data'}}]}.
s26(internal, {'Rcpt'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s26 Sending Rcpt to S ~n", []),
    gen_c:send_s26_Rcpt(SPid, Data),
    {next_state, s27, Data};
s26(internal, {'Bogus'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s26 Sending Bogus to S ~n", []),
    gen_c:send_s26_Bogus(SPid, Data),
    {next_state, s32, Data, [{next_event, internal, {'Data'}}]}.

s27(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    case make_choice_s26(Data) of
        1 ->
            {next_state, s26, Data, [{next_event, internal, {'Bogus'}}]};
        2 ->
            {next_state, s26, Data, [{next_event, internal, {'Rcpt'}}]}
    end.

-spec make_choice_s12(state_data()) -> integer().
make_choice_s12(_Data) ->
    rand:uniform(2).

-spec make_choice_s10(state_data()) -> integer().
make_choice_s10(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("c connected ~n", []),
    SPid = case whereis(s) of
        undefined ->
            io:format("s is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(s);
        Pid_s ->
            Pid_s
    end,
    Data#state_data{s_pid = SPid}.

