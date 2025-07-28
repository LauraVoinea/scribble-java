-module(gen_s).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s1_220/2, 
	 s1/3, 
	 send_s5_Timeout/2, 
	 s5/3, 
	 send_s6_EhloCommit/2, 
	 s6/3, 
	 send_s8_250d/2, 
	 s8/3, 
	 send_s8_250/2, 
	 s11/3, 
	 send_s12_220/2, 
	 s12/3, 
	 s13/3, 
	 send_s15_250d/2, 
	 s15/3, 
	 send_s15_250/2, 
	 s19/3, 
	 send_s20_535/2, 
	 s20/3, 
	 send_s20_235/2, 
	 s22/3, 
	 send_s23_250/2, 
	 s23/3, 
	 send_s23_501/2, 
	 s27/3, 
	 send_s28_250/2, 
	 s28/3, 
	 send_s33_Timeout/2, 
	 s33/3, 
	 send_s34_354/2, 
	 s34/3, 
	 s36/3, 
	 send_s41_250/2, 
	 s41/3, 
	 send_s44_221/2, 
	 s44/3, 
	 send_s49_Ack/2, 
	 s49/3, 
	 send_s51_Ack/2, 
	 s51/3, 
	 send_s53_AckCommit/2, 
	 s53/3
	 ]).

-include("s.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), c_pid :: pid() | undefined}.

-callback s51(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s53(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s33(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {keep_state, state_data(), [postpone]} | {next_state, s34, state_data(), [{next_event, internal, {'354'}}]} | {keep_state, state_data()}.
-callback s11(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s51, state_data(), [{next_event, internal, {'Ack'}}]} | {keep_state, state_data()} | {next_state, s12, state_data(), [{next_event, internal, {'220'}}]}.
-callback s13(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s49, state_data(), [{next_event, internal, {'Ack'}}]} | {keep_state, state_data()} | {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} | {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]}.
-callback s34(EventType :: term(), {atom()}, state_data()) -> {next_state, s36, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {next_state, s13, state_data()}.
-callback s15(EventType :: term(), {atom()}, state_data()) -> {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} | {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]} | {keep_state, state_data()} | {next_state, s19, state_data()}.
-callback s36(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s36, state_data()} | {next_state, s41, state_data(), [{next_event, internal, {'250'}}]} | {keep_state, state_data()}.
-callback s19(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()} | {next_state, s20, state_data(), [{next_event, internal, {'535'}}]} | {next_state, s20, state_data(), [{next_event, internal, {'235'}}]} | {keep_state, state_data()}.
-callback s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {'Timeout'}}]} | {keep_state, state_data()}.
-callback s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {keep_state, state_data(), [postpone]} | {next_state, s6, state_data(), [{next_event, internal, {'EhloCommit'}}]} | {keep_state, state_data()} | {next_state, s53, state_data(), [{next_event, internal, {'AckCommit'}}]}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {next_state, s8, state_data(), [{next_event, internal, {'250d'}}]} | {next_state, s8, state_data(), [{next_event, internal, {'250'}}]} | {keep_state, state_data()}.
-callback s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} | {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]} | {keep_state, state_data()} | {next_state, s11, state_data()}.
-callback s20(EventType :: term(), {atom()}, state_data()) -> {next_state, s19, state_data()} | {next_state, s22, state_data()}.
-callback s41(EventType :: term(), {atom()}, state_data()) -> {next_state, s22, state_data()}.
-callback s44(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s22(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s23, state_data(), [{next_event, internal, {'250'}}]} | {next_state, s23, state_data(), [{next_event, internal, {'501'}}]} | {keep_state, state_data()} | {next_state, s44, state_data(), [{next_event, internal, {'221'}}]}.
-callback s23(EventType :: term(), {atom()}, state_data()) -> {next_state, s27, state_data()} | {next_state, s22, state_data()}.
-callback s28(EventType :: term(), {atom()}, state_data()) -> {next_state, s27, state_data()}.
-callback s49(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s27(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s28, state_data(), [{next_event, internal, {'250'}}]} | {keep_state, state_data()} | {next_state, s33, state_data(), [{next_event, internal, {'Timeout'}}]}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {'220'}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_s, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "s_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {'220'}}]}.
init({CallbackModule, _Args}) ->
    io:format("s: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s6_EhloCommit(CPid :: pid(), Data :: state_data()) -> ok.
send_s6_EhloCommit(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'EhloCommit'}, Counter}).

-spec send_s8_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s8_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec s51(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s51(EventType, {'Ack'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s51(EventType, {'Ack'}, Data).

-spec s53(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s53(EventType, {'AckCommit'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s53(EventType, {'AckCommit'}, Data).

-spec send_s23_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s23_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec s33(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {next_state, s34, state_data(), [{next_event, internal, {'354'}}]} |
    {keep_state, state_data()}.
s33(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s33(EventType, {'Timeout'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s33(EventType, {'Timeout'}, NewData);
s33(EventType, {CPid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s33(EventType, {CPid, {'Data'}}, Data);
s33(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s11(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s51, state_data(), [{next_event, internal, {'Ack'}}]} |
    {keep_state, state_data()} |
    {next_state, s12, state_data(), [{next_event, internal, {'220'}}]}.
s11(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Ehlo1'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Ehlo1']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Auth'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Auth']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s11(_EventType, {_Pid, {'StartTls'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['StartTls']]),
    {keep_state, Data, [postpone]};
s11(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {CPid, {'Quit'}}, Data);
s11(EventType, {CPid, {'StartTls'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {CPid, {'StartTls'}}, Data);
s11(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Ehlo1'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s13(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s49, state_data(), [{next_event, internal, {'Ack'}}]} |
    {keep_state, state_data()} |
    {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]}.
s13(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Auth'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Auth']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s13(_EventType, {_Pid, {'Ehlo1'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Ehlo1']]),
    {keep_state, Data, [postpone]};
s13(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {CPid, {'Quit'}}, Data);
s13(EventType, {CPid, {'Ehlo1'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {CPid, {'Ehlo1'}}, Data);
s13(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Auth'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Ehlo1'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s34(EventType :: term(), {atom()}, state_data()) -> {next_state, s36, state_data()}.
s34(EventType, {'354'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s34(EventType, {'354'}, Data).

-spec s12(EventType :: term(), {atom()}, state_data()) -> {next_state, s13, state_data()}.
s12(EventType, {'220'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {'220'}, Data).

-spec s15(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]} |
    {keep_state, state_data()} |
    {next_state, s19, state_data()}.
s15(EventType, {'250d'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {'250d'}, Data);
s15(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {'250'}, Data).

-spec send_s12_220(CPid :: pid(), Data :: state_data()) -> ok.
send_s12_220(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'220'}, Counter}).

-spec s36(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s36, state_data()} |
    {next_state, s41, state_data(), [{next_event, internal, {'250'}}]} |
    {keep_state, state_data()}.
s36(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s36(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s36(EventType, {CPid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s36(EventType, {CPid, {'Subject'}}, Data);
s36(EventType, {CPid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s36(EventType, {CPid, {'DataLine'}}, Data);
s36(EventType, {CPid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s36(EventType, {CPid, {'EndOfData'}}, Data);
s36(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s19(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()} |
    {next_state, s20, state_data(), [{next_event, internal, {'535'}}]} |
    {next_state, s20, state_data(), [{next_event, internal, {'235'}}]} |
    {keep_state, state_data()}.
s19(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s19(_EventType, {_Pid, {'Auth'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Auth']]),
    {keep_state, Data, [postpone]};
s19(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {CPid, {'Quit'}}, Data);
s19(EventType, {CPid, {'Auth'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {CPid, {'Auth'}}, Data);
s19(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Auth'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s1(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s5, state_data(), [{next_event, internal, {'Timeout'}}]} |
    {keep_state, state_data()}.
s1(EventType, {'220'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {'220'}, Data).

-spec send_s53_AckCommit(CPid :: pid(), Data :: state_data()) -> ok.
send_s53_AckCommit(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'AckCommit'}, Counter}).

-spec send_s51_Ack(CPid :: pid(), Data :: state_data()) -> ok.
send_s51_Ack(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Ack'}, Counter}).

-spec send_s20_535(CPid :: pid(), Data :: state_data()) -> ok.
send_s20_535(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'535'}, Counter}).

-spec s5(EventType :: term(), {atom()} | {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {next_state, s6, state_data(), [{next_event, internal, {'EhloCommit'}}]} |
    {keep_state, state_data()} |
    {next_state, s53, state_data(), [{next_event, internal, {'AckCommit'}}]}.
s5(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Ehlo1'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Ehlo1']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Auth'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Auth']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'StartTls'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['StartTls']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Ehlo'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Ehlo']]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'QuitCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['QuitCommit']]),
    {keep_state, Data, [postpone]};
s5(EventType, {'Timeout'}, #state_data{mc_counter_2 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {'Timeout'}, NewData);
s5(EventType, {CPid, {'Ehlo'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {CPid, {'Ehlo'}}, Data);
s5(EventType, {CPid, {'QuitCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {CPid, {'QuitCommit'}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'QuitCommit'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Ehlo1'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s8, state_data(), [{next_event, internal, {'250d'}}]} |
    {next_state, s8, state_data(), [{next_event, internal, {'250'}}]} |
    {keep_state, state_data()}.
s6(EventType, {'EhloCommit'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {'EhloCommit'}, Data).

-spec s8(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s15, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s15, state_data(), [{next_event, internal, {'250d'}}]} |
    {keep_state, state_data()} |
    {next_state, s11, state_data()}.
s8(EventType, {'250d'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {'250d'}, Data);
s8(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {'250'}, Data).

-spec send_s33_Timeout(CPid :: pid(), Data :: state_data()) -> ok.
send_s33_Timeout(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'Timeout'}, Counter}).

-spec send_s15_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s15_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec send_s34_354(CPid :: pid(), Data :: state_data()) -> ok.
send_s34_354(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'354'}, Counter}).

-spec send_s41_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s41_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec send_s23_501(CPid :: pid(), Data :: state_data()) -> ok.
send_s23_501(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'501'}, Counter}).

-spec s20(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s19, state_data()} |
    {next_state, s22, state_data()}.
s20(EventType, {'535'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s20(EventType, {'535'}, Data);
s20(EventType, {'235'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s20(EventType, {'235'}, Data).

-spec s41(EventType :: term(), {atom()}, state_data()) -> {next_state, s22, state_data()}.
s41(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s41(EventType, {'250'}, Data).

-spec send_s28_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s28_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec send_s15_250d(CPid :: pid(), Data :: state_data()) -> ok.
send_s15_250d(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250d'}, Counter}).

-spec send_s49_Ack(CPid :: pid(), Data :: state_data()) -> ok.
send_s49_Ack(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Ack'}, Counter}).

-spec s44(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s44(EventType, {'221'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s44(EventType, {'221'}, Data).

-spec s22(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s23, state_data(), [{next_event, internal, {'250'}}]} |
    {next_state, s23, state_data(), [{next_event, internal, {'501'}}]} |
    {keep_state, state_data()} |
    {next_state, s44, state_data(), [{next_event, internal, {'221'}}]}.
s22(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s22(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s22(EventType, {CPid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {CPid, {'Mail'}}, Data);
s22(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {CPid, {'Quit'}}, Data);
s22(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s23(EventType :: term(), {atom()}, state_data()) ->
    {next_state, s27, state_data()} |
    {next_state, s22, state_data()}.
s23(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s23(EventType, {'250'}, Data);
s23(EventType, {'501'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s23(EventType, {'501'}, Data).

-spec send_s8_250d(CPid :: pid(), Data :: state_data()) -> ok.
send_s8_250d(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250d'}, Counter}).

-spec s28(EventType :: term(), {atom()}, state_data()) -> {next_state, s27, state_data()}.
s28(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s28(EventType, {'250'}, Data).

-spec send_s1_220(CPid :: pid(), _Data :: state_data()) -> ok.
send_s1_220(CPid, _Data) ->
    gen_statem:cast(CPid, {self(), {'220'}}).

-spec s49(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s49(EventType, {'Ack'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s49(EventType, {'Ack'}, Data).

-spec s27(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s28, state_data(), [{next_event, internal, {'250'}}]} |
    {keep_state, state_data()} |
    {next_state, s33, state_data(), [{next_event, internal, {'Timeout'}}]}.
s27(_EventType, {_Pid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['DataLine']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Data']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Quit']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['EndOfData']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Subject']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_s: Postponing event ~p~n", [['Mail']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Bogus']]),
    {keep_state, Data, [postpone]};
s27(_EventType, {_Pid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_s: Postponing event ~p~n", [['Rcpt']]),
    {keep_state, Data, [postpone]};
s27(EventType, {CPid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {CPid, {'Rcpt'}}, Data);
s27(EventType, {CPid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {CPid, {'Bogus'}}, Data);
s27(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Mail'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'DataLine'} ->
    io:format("gen_s: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s5_Timeout(CPid :: pid(), Data :: state_data()) -> ok.
send_s5_Timeout(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Timeout'}, Counter}).

-spec send_s44_221(CPid :: pid(), Data :: state_data()) -> ok.
send_s44_221(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'221'}, Counter}).

-spec send_s20_235(CPid :: pid(), Data :: state_data()) -> ok.
send_s20_235(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'235'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

