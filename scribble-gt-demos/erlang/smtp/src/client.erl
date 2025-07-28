-module(client).
-behaviour(gen_c).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s1/3,
	 make_choice_s5/1,
	 s5/3,
	 s6/3,
	 s8/3,
	 make_choice_s11/1,
	 s11/3,
	 s12/3,
	 make_choice_s13/1,
	 s13/3,
	 s15/3,
	 make_choice_s19/1,
	 s19/3,
	 s20/3,
	 make_choice_s22/1,
	 s22/3,
	 s23/3,
	 make_choice_s27/1,
	 s27/3,
	 s28/3,
	 s33/3,
	 s34/3,
	 make_choice_s36/1,
	 s36/3,
	 s41/3,
	 s44/3,
	 s49/3,
	 s51/3,
	 s53/3
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
    Data = #state_data{mc_counter_1 = 0, mc_counter_2 = 0},
    io:format("c initialized ~n", []),
    {ok, s1, Data}.

-spec make_choice_s5(state_data()) -> integer().
make_choice_s5(_Data) ->
    rand:uniform(2).

-spec s51(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s51(cast, {SPid, {'Ack'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s51 Received Ack  from S ~p ~n", [SPid]),
    {stop, normal, Data}.

-spec s53(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s53(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s53 Received Timeout  from S ~p ~n", [SPid]),
    {stop, normal, Data};
s53(cast, {SPid, {'AckCommit'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s53 Received AckCommit  from S ~p ~n", [SPid]),
    {stop, normal, Data}.

-spec s33(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s34, state_data()} |
    {stop, normal, state_data()}.
s33(internal, {'Data'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s33 Sending Data to S ~n", []),
    gen_c:send_s33_Data(SPid, Data),
    {next_state, s34, Data};
s33(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s33 Received Timeout  from S ~p ~n", [SPid]),
    {stop, normal, Data}.

-spec s11(internal, {atom()}, state_data()) ->
    {next_state, s12, state_data()} |
    {next_state, s51, state_data()}.
s11(internal, {'StartTls'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s11 Sending StartTls to S ~n", []),
    gen_c:send_s11_StartTls(SPid, Data),
    {next_state, s12, Data};
s11(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s11 Sending Quit to S ~n", []),
    gen_c:send_s11_Quit(SPid, Data),
    {next_state, s51, Data}.

-spec s13(internal, {atom()}, state_data()) ->
    {next_state, s49, state_data()} |
    {next_state, s15, state_data()}.
s13(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s13 Sending Quit to S ~n", []),
    gen_c:send_s13_Quit(SPid, Data),
    {next_state, s49, Data};
s13(internal, {'Ehlo1'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s13 Sending Ehlo1 to S ~n", []),
    gen_c:send_s13_Ehlo1(SPid, Data),
    {next_state, s15, Data}.

-spec s34(cast, {pid(), {atom(), term()}}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s36, state_data(), [{next_event, internal, {'EndOfData'}}]} |
    {next_state, s36, state_data(), [{next_event, internal, {'DataLine'}}]} |
    {next_state, s36, state_data(), [{next_event, internal, {'Subject'}}]} |
    {keep_state, state_data()}.
s34(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s34 Received Timeout  from S ~p ~n", [SPid]),
    {stop, normal, Data};
s34(cast, {SPid, {'354'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s34 Received 354  from S ~p ~n", [SPid]),
    case make_choice_s36(Data) of
        1 ->
            {next_state, s36, Data, [{next_event, internal, {'EndOfData'}}]};
        2 ->
            {next_state, s36, Data, [{next_event, internal, {'DataLine'}}]};
        3 ->
            {next_state, s36, Data, [{next_event, internal, {'Subject'}}]}
    end.

-spec make_choice_s27(state_data()) -> integer().
make_choice_s27(_Data) ->
    rand:uniform(2).

-spec s12(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s13, state_data(), [{next_event, internal, {'Quit'}}]} |
    {next_state, s13, state_data(), [{next_event, internal, {'Ehlo1'}}]} |
    {keep_state, state_data()}.
s12(cast, {SPid, {'220'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s12 Received 220  from S ~p ~n", [SPid]),
    case make_choice_s13(Data) of
        1 ->
            {next_state, s13, Data, [{next_event, internal, {'Quit'}}]};
        2 ->
            {next_state, s13, Data, [{next_event, internal, {'Ehlo1'}}]}
    end.

-spec s15(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s15, state_data()} |
    {next_state, s19, state_data(), [{next_event, internal, {'Auth'}}]} |
    {next_state, s19, state_data(), [{next_event, internal, {'Quit'}}]} |
    {keep_state, state_data()}.
s15(cast, {SPid, {'250d'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s15 Received 250d  from S ~p ~n", [SPid]),
    {next_state, s15, Data};
s15(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s15 Received 250  from S ~p ~n", [SPid]),
    case make_choice_s19(Data) of
        1 ->
            {next_state, s19, Data, [{next_event, internal, {'Auth'}}]};
        2 ->
            {next_state, s19, Data, [{next_event, internal, {'Quit'}}]}
    end.

-spec s36(internal, {atom()}, state_data()) ->
    {next_state, s36, state_data(), [{next_event, internal, {'EndOfData'}}]} |
    {next_state, s36, state_data(), [{next_event, internal, {'DataLine'}}]} |
    {next_state, s36, state_data(), [{next_event, internal, {'Subject'}}]} |
    {keep_state, state_data()} |
    {next_state, s41, state_data()}.
s36(internal, {'DataLine'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s36 Sending DataLine to S ~n", []),
    gen_c:send_s36_DataLine(SPid, Data),
    case make_choice_s36(Data) of
        1 ->
            {next_state, s36, Data, [{next_event, internal, {'EndOfData'}}]};
        2 ->
            {next_state, s36, Data, [{next_event, internal, {'DataLine'}}]};
        3 ->
            {next_state, s36, Data, [{next_event, internal, {'Subject'}}]}
    end;
s36(internal, {'EndOfData'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s36 Sending EndOfData to S ~n", []),
    gen_c:send_s36_EndOfData(SPid, Data),
    {next_state, s41, Data};
s36(internal, {'Subject'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s36 Sending Subject to S ~n", []),
    gen_c:send_s36_Subject(SPid, Data),
    case make_choice_s36(Data) of
        1 ->
            {next_state, s36, Data, [{next_event, internal, {'EndOfData'}}]};
        2 ->
            {next_state, s36, Data, [{next_event, internal, {'DataLine'}}]};
        3 ->
            {next_state, s36, Data, [{next_event, internal, {'Subject'}}]}
    end.

-spec make_choice_s22(state_data()) -> integer().
make_choice_s22(_Data) ->
    rand:uniform(2).

-spec s19(internal, {atom()}, state_data()) ->
    {next_state, s20, state_data()} |
    {stop, normal, state_data()}.
s19(internal, {'Auth'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s19 Sending Auth to S ~n", []),
    gen_c:send_s19_Auth(SPid, Data),
    {next_state, s20, Data};
s19(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s19 Sending Quit to S ~n", []),
    gen_c:send_s19_Quit(SPid, Data),
    {stop, normal, Data}.

-spec s1(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s5, state_data(), [{next_event, internal, {'Ehlo'}}]} |
    {next_state, s5, state_data(), [{next_event, internal, {'QuitCommit'}}]} |
    {keep_state, state_data()}.
s1(cast, {SPid, {'220'}}, Data) ->
    NewData = Data#state_data{s_pid = SPid},
    io:format("C: connection ~p ~n", [NewData]),
    io:format("C: s1 Received 220  from S ~p ~n", [SPid]),
    case make_choice_s5(Data) of
        1 ->
            {next_state, s5, NewData, [{next_event, internal, {'Ehlo'}}]};
        2 ->
            {next_state, s5, NewData, [{next_event, internal, {'QuitCommit'}}]}
    end.

-spec s5(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s6, state_data()} |
    {next_state, s53, state_data()} |
    {stop, normal, state_data()}.
s5(internal, {'Ehlo'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s5 Sending Ehlo to S ~n", []),
    gen_c:send_s5_Ehlo(SPid, Data),
    {next_state, s6, Data};
s5(internal, {'QuitCommit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s5 Sending QuitCommit to S ~n", []),
    gen_c:send_s5_QuitCommit(SPid, Data),
    {next_state, s53, Data};
s5(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s5 Received Timeout  from S ~p ~n", [SPid]),
    {stop, normal, Data}.

-spec s6(cast, {pid(), {atom(), term()}}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s8, state_data()}.
s6(cast, {SPid, {'Timeout'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s6 Received Timeout  from S ~p ~n", [SPid]),
    {stop, normal, Data};
s6(cast, {SPid, {'EhloCommit'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s6 Received EhloCommit  from S ~p ~n", [SPid]),
    {next_state, s8, Data}.

-spec s8(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s11, state_data(), [{next_event, internal, {'StartTls'}}]} |
    {next_state, s11, state_data(), [{next_event, internal, {'Quit'}}]} |
    {keep_state, state_data()} |
    {next_state, s15, state_data()}.
s8(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s8 Received 250  from S ~p ~n", [SPid]),
    case make_choice_s11(Data) of
        1 ->
            {next_state, s11, Data, [{next_event, internal, {'StartTls'}}]};
        2 ->
            {next_state, s11, Data, [{next_event, internal, {'Quit'}}]}
    end;
s8(cast, {SPid, {'250d'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s8 Received 250d  from S ~p ~n", [SPid]),
    {next_state, s15, Data}.

-spec s20(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s19, state_data(), [{next_event, internal, {'Auth'}}]} |
    {next_state, s19, state_data(), [{next_event, internal, {'Quit'}}]} |
    {keep_state, state_data()} |
    {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} |
    {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]}.
s20(cast, {SPid, {'535'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s20 Received 535  from S ~p ~n", [SPid]),
    case make_choice_s19(Data) of
        1 ->
            {next_state, s19, Data, [{next_event, internal, {'Auth'}}]};
        2 ->
            {next_state, s19, Data, [{next_event, internal, {'Quit'}}]}
    end;
s20(cast, {SPid, {'235'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s20 Received 235  from S ~p ~n", [SPid]),
    case make_choice_s22(Data) of
        1 ->
            {next_state, s22, Data, [{next_event, internal, {'Mail'}}]};
        2 ->
            {next_state, s22, Data, [{next_event, internal, {'Quit'}}]}
    end.

-spec make_choice_s19(state_data()) -> integer().
make_choice_s19(_Data) ->
    rand:uniform(2).

-spec s41(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} |
    {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]} |
    {keep_state, state_data()}.
s41(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s41 Received 250  from S ~p ~n", [SPid]),
    case make_choice_s22(Data) of
        1 ->
            {next_state, s22, Data, [{next_event, internal, {'Mail'}}]};
        2 ->
            {next_state, s22, Data, [{next_event, internal, {'Quit'}}]}
    end.

-spec s44(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s44(cast, {SPid, {'221'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s44 Received 221  from S ~p ~n", [SPid]),
    {stop, normal, Data}.

-spec s22(internal, {atom()}, state_data()) ->
    {next_state, s23, state_data()} |
    {next_state, s44, state_data()}.
s22(internal, {'Mail'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s22 Sending Mail to S ~n", []),
    gen_c:send_s22_Mail(SPid, Data),
    {next_state, s23, Data};
s22(internal, {'Quit'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s22 Sending Quit to S ~n", []),
    gen_c:send_s22_Quit(SPid, Data),
    {next_state, s44, Data}.

-spec s23(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s27, state_data(), [{next_event, internal, {'Bogus'}}]} |
    {next_state, s27, state_data(), [{next_event, internal, {'Rcpt'}}]} |
    {keep_state, state_data()} |
    {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} |
    {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]}.
s23(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s23 Received 250  from S ~p ~n", [SPid]),
    case make_choice_s27(Data) of
        1 ->
            {next_state, s27, Data, [{next_event, internal, {'Bogus'}}]};
        2 ->
            {next_state, s27, Data, [{next_event, internal, {'Rcpt'}}]}
    end;
s23(cast, {SPid, {'501'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s23 Received 501  from S ~p ~n", [SPid]),
    case make_choice_s22(Data) of
        1 ->
            {next_state, s22, Data, [{next_event, internal, {'Mail'}}]};
        2 ->
            {next_state, s22, Data, [{next_event, internal, {'Quit'}}]}
    end.

-spec make_choice_s13(state_data()) -> integer().
make_choice_s13(_Data) ->
    rand:uniform(2).

-spec make_choice_s36(state_data()) -> integer().
make_choice_s36(_Data) ->
    rand:uniform(3).

-spec s28(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s27, state_data(), [{next_event, internal, {'Bogus'}}]} |
    {next_state, s27, state_data(), [{next_event, internal, {'Rcpt'}}]} |
    {keep_state, state_data()}.
s28(cast, {SPid, {'250'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s28 Received 250  from S ~p ~n", [SPid]),
    case make_choice_s27(Data) of
        1 ->
            {next_state, s27, Data, [{next_event, internal, {'Bogus'}}]};
        2 ->
            {next_state, s27, Data, [{next_event, internal, {'Rcpt'}}]}
    end.

-spec make_choice_s11(state_data()) -> integer().
make_choice_s11(_Data) ->
    rand:uniform(2).

-spec s49(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s49(cast, {SPid, {'Ack'}}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s49 Received Ack  from S ~p ~n", [SPid]),
    {stop, normal, Data}.

-spec s27(internal, {atom()}, state_data()) ->
    {next_state, s33, state_data(), [{next_event, internal, {'Data'}}]} |
    {keep_state, state_data()} |
    {next_state, s28, state_data()}.
s27(internal, {'Bogus'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s27 Sending Bogus to S ~n", []),
    gen_c:send_s27_Bogus(SPid, Data),
    {next_state, s33, Data, [{next_event, internal, {'Data'}}]};
s27(internal, {'Rcpt'}, #state_data{s_pid = SPid} = Data) ->
    io:format("C: s27 Sending Rcpt to S ~n", []),
    gen_c:send_s27_Rcpt(SPid, Data),
    {next_state, s28, Data}.