-module(gen_c).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s1/3, 
	 send_s5_Quit/2, 
	 s5/3, 
	 send_s5_Ehlo/2, 
	 s7/3, 
	 send_s10_StartTls/2, 
	 s10/3, 
	 send_s10_Quit/2, 
	 s11/3, 
	 send_s12_Quit/2, 
	 s12/3, 
	 send_s12_Ehlo/2, 
	 s14/3, 
	 send_s18_Quit/2, 
	 s18/3, 
	 send_s18_Auth/2, 
	 s19/3, 
	 send_s21_Mail/2, 
	 s21/3, 
	 send_s21_Quit/2, 
	 s22/3, 
	 send_s26_Rcpt/2, 
	 s26/3, 
	 send_s26_Bogus/2, 
	 s27/3, 
	 send_s32_Data/2, 
	 s32/3, 
	 s33/3, 
	 send_s35_EndOfData/2, 
	 s35/3, 
	 send_s35_DataLine/2, 
	 send_s35_Subject/2, 
	 s40/3, 
	 s43/3, 
	 s48/3, 
	 s50/3, 
	 s52/3
	 ]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), s_pid :: pid() | undefined}.

-callback s50(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s52(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s33(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s11(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s32(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s33, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s10(EventType :: term(), {atom()}, state_data()) -> {next_state, s11, state_data()} | {next_state, s50, state_data()}.
-callback s35(EventType :: term(), {atom()}, state_data()) -> {next_state, s40, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {next_state, s48, state_data()} | {next_state, s14, state_data()}.
-callback s14(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data()} | {keep_state, state_data()}.
-callback s19(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s18(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()} | {next_state, s19, state_data()}.
-callback s5(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s52, state_data()} | {next_state, s7, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s7(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s40(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s22(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s43(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s21(EventType :: term(), {atom()}, state_data()) -> {next_state, s22, state_data()} | {next_state, s43, state_data()}.
-callback s48(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s26(EventType :: term(), {atom()}, state_data()) -> {next_state, s27, state_data()}.
-callback s27(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args}, 
			% []);
			[{debug, [trace, {log_to_file, "c_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("c: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s26_Bogus(SPid :: pid(), Data :: state_data()) -> ok.
send_s26_Bogus(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Bogus'}, Counter}).

-spec send_s10_StartTls(SPid :: pid(), Data :: state_data()) -> ok.
send_s10_StartTls(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'StartTls'}, Counter}).

-spec send_s26_Rcpt(SPid :: pid(), Data :: state_data()) -> ok.
send_s26_Rcpt(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Rcpt'}, Counter}).

-spec send_s35_DataLine(SPid :: pid(), Data :: state_data()) -> ok.
send_s35_DataLine(SPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SPid, {self(), {'DataLine'}, Counter}).

-spec send_s18_Auth(SPid :: pid(), Data :: state_data()) -> ok.
send_s18_Auth(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Auth'}, Counter}).

-spec s50(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s50(EventType, {SPid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s50(EventType, {SPid, {'Ack'}}, Data);
s50(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec send_s32_Data(SPid :: pid(), Data :: state_data()) -> ok.
send_s32_Data(SPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SPid, {self(), {'Data'}, Counter}).

-spec s52(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s52(EventType, {SPid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s52(EventType, {SPid, {'Ack'}}, Data);
s52(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s52(EventType, {SPid, {'Timeout'}}, Data);
s52(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec send_s35_Subject(SPid :: pid(), Data :: state_data()) -> ok.
send_s35_Subject(SPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SPid, {self(), {'Subject'}, Counter}).

-spec s33(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s33(EventType, {SPid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s33(EventType, {SPid, {'354'}}, Data);
s33(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s33(EventType, {SPid, {'Timeout'}}, Data);
s33(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s11(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s11(EventType, {SPid, {'220'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {SPid, {'220'}}, Data);
s11(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s32(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s33, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s32(EventType, {'Data'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s32(EventType, {'Data'}, NewData);
s32(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s32(EventType, {SPid, {'Timeout'}}, Data);
s32(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s10(EventType :: term(), {atom()}, state_data()) -> {next_state, s11, state_data()} | {next_state, s50, state_data()}.
s10(EventType, {'StartTls'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {'StartTls'}, Data);
s10(EventType, {'Quit'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {'Quit'}, Data).

-spec s35(EventType :: term(), {atom()}, state_data()) -> {next_state, s40, state_data()}.
s35(EventType, {'EndOfData'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s35(EventType, {'EndOfData'}, Data);
s35(EventType, {'DataLine'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s35(EventType, {'DataLine'}, Data);
s35(EventType, {'Subject'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s35(EventType, {'Subject'}, Data).

-spec s12(EventType :: term(), {atom()}, state_data()) -> {next_state, s48, state_data()} | {next_state, s14, state_data()}.
s12(EventType, {'Quit'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {'Quit'}, Data);
s12(EventType, {'Ehlo'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {'Ehlo'}, Data).

-spec send_s5_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s5_Quit(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec send_s5_Ehlo(SPid :: pid(), Data :: state_data()) -> ok.
send_s5_Ehlo(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Ehlo'}, Counter}).

-spec s14(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data()} | {keep_state, state_data()}.
s14(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {SPid, {'250'}}, Data);
s14(EventType, {SPid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {SPid, {'250d'}}, Data);
s14(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec send_s21_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s21_Quit(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec s19(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s19(EventType, {SPid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {SPid, {'235'}}, Data);
s19(EventType, {SPid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {SPid, {'535'}}, Data);
s19(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec send_s12_Ehlo(SPid :: pid(), Data :: state_data()) -> ok.
send_s12_Ehlo(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Ehlo'}, Counter}).

-spec s18(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()} | {next_state, s19, state_data()}.
s18(EventType, {'Quit'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {'Quit'}, Data);
s18(EventType, {'Auth'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {'Auth'}, Data).

-spec send_s35_EndOfData(SPid :: pid(), Data :: state_data()) -> ok.
send_s35_EndOfData(SPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SPid, {self(), {'EndOfData'}, Counter}).

-spec send_s10_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s10_Quit(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

s1(EventType, {SPid, {'220'}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {SPid, {'220'}}, Data).

-spec s5(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s52, state_data()} | {next_state, s7, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s5(EventType, {'Quit'}, #state_data{mc_counter_2 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {'Quit'}, NewData);
s5(EventType, {'Ehlo'}, #state_data{mc_counter_2 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {'Ehlo'}, NewData);
s5(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {SPid, {'Timeout'}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s7(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s14, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s7(EventType, {SPid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {SPid, {'250d'}}, Data);
s7(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {SPid, {'Timeout'}}, Data);
s7(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {SPid, {'250'}}, Data);
s7(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec send_s12_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s12_Quit(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec s40(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s40(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s40(EventType, {SPid, {'250'}}, Data);
s40(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s22(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s22(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {SPid, {'250'}}, Data);
s22(EventType, {SPid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {SPid, {'501'}}, Data);
s22(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s43(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s43(EventType, {SPid, {221}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s43(EventType, {SPid, {221}}, Data);
s43(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s21(EventType :: term(), {atom()}, state_data()) -> {next_state, s22, state_data()} | {next_state, s43, state_data()}.
s21(EventType, {'Mail'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s21(EventType, {'Mail'}, Data);
s21(EventType, {'Quit'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s21(EventType, {'Quit'}, Data).

-spec s48(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s48(EventType, {SPid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s48(EventType, {SPid, {'Ack'}}, Data);
s48(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec s26(EventType :: term(), {atom()}, state_data()) -> {next_state, s27, state_data()}.
s26(EventType, {'Rcpt'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s26(EventType, {'Rcpt'}, Data);
s26(EventType, {'Bogus'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s26(EventType, {'Bogus'}, Data).

-spec s27(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s27(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {SPid, {'250'}}, Data);
s27(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'235'} 
		orelse Msg =:= {'250d'} 
		orelse Msg =:= {'354'} 
		orelse Msg =:= {'501'} 
		orelse Msg =:= {'535'} 
		orelse Msg =:= {'220'} 
		orelse Msg =:= {'Ack'} 
		orelse Msg =:= {'221'} 
		orelse Msg =:= {'250'} 
		orelse Msg =:= {'Timeout'} ->
    {keep_state, Data}.

-spec send_s21_Mail(SPid :: pid(), Data :: state_data()) -> ok.
send_s21_Mail(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Mail'}, Counter}).

-spec send_s18_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s18_Quit(SPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

