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
	 send_s7_250/2, 
	 s7/3, 
	 send_s7_250d/2, 
	 s10/3, 
	 send_s11_220/2, 
	 s11/3, 
	 s12/3, 
	 send_s14_250d/2, 
	 s14/3, 
	 send_s14_250/2, 
	 s18/3, 
	 send_s19_535/2, 
	 s19/3, 
	 send_s19_235/2, 
	 s21/3, 
	 send_s22_250/2, 
	 s22/3, 
	 send_s22_501/2, 
	 s26/3, 
	 send_s27_250/2, 
	 s27/3, 
	 send_s32_Timeout/2, 
	 s32/3, 
	 send_s33_354/2, 
	 s33/3, 
	 s35/3, 
	 send_s40_250/2, 
	 s40/3, 
	 send_s43_221/2, 
	 s43/3, 
	 send_s48_Ack/2, 
	 s48/3, 
	 send_s50_Ack/2, 
	 s50/3, 
	 send_s52_Ack/2, 
	 s52/3
	 ]).

-include("s.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), c_pid :: pid() | undefined}.

-callback s50(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s52(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s33(EventType :: term(), {atom()}, state_data()) -> {next_state, s35, state_data()}.
-callback s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data()}.
-callback s32(EventType :: term(), {atom()} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s10(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s35(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s35, state_data()} | {next_state, s35, state_data()} | {keep_state, state_data()}.
-callback s12(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s14(EventType :: term(), {atom()}, state_data()) -> {next_state, s18, state_data()}.
-callback s19(EventType :: term(), {atom()}, state_data()) -> {next_state, s18, state_data()} | {next_state, s21, state_data()}.
-callback s18(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s5(EventType :: term(), {atom()} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {next_state, s10, state_data()}.
-callback s40(EventType :: term(), {atom()}, state_data()) -> {next_state, s21, state_data()}.
-callback s22(EventType :: term(), {atom()}, state_data()) -> {next_state, s26, state_data()} | {next_state, s21, state_data()}.
-callback s43(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s21(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s48(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s26(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s27(EventType :: term(), {atom()}, state_data()) -> {next_state, s26, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s1, state_data(), [{next_event, internal, {220}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_s, {CallbackModule, Args}, 
            % []);
            [{debug, [trace, {log_to_file, "s_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s1, state_data(), [{next_event, internal, {220}}]}.
init({CallbackModule, _Args}) ->
    io:format("s: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec send_s7_250d(CPid :: pid(), Data :: state_data()) -> ok.
send_s7_250d(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250d'}, Counter}).

-spec send_s19_235(CPid :: pid(), Data :: state_data()) -> ok.
send_s19_235(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'235'}, Counter}).

-spec send_s14_250d(CPid :: pid(), Data :: state_data()) -> ok.
send_s14_250d(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250d'}, Counter}).

-spec send_s11_220(CPid :: pid(), Data :: state_data()) -> ok.
send_s11_220(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'220'}, Counter}).

-spec send_s22_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s22_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec send_s19_535(CPid :: pid(), Data :: state_data()) -> ok.
send_s19_535(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'535'}, Counter}).

-spec send_s50_Ack(CPid :: pid(), Data :: state_data()) -> ok.
send_s50_Ack(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Ack'}, Counter}).

-spec s50(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s50(EventType, {'Ack'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s50(EventType, {'Ack'}, Data).

-spec s52(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s52(EventType, {'Ack'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s52(EventType, {'Ack'}, Data).

-spec s33(EventType :: term(), {atom()}, state_data()) -> {next_state, s35, state_data()}.
s33(EventType, {'354'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s33(EventType, {'354'}, Data).

-spec s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data()}.
s11(EventType, {'220'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {'220'}, Data).

-spec s32(EventType :: term(), {atom()} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s32(EventType, {'Timeout'}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s32(EventType, {'Timeout'}, NewData);
s32(EventType, {CPid, {'Data'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s32(EventType, {CPid, {'Data'}}, Data);
s32(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec s10(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s10(EventType, {CPid, {'StartTls'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {CPid, {'StartTls'}}, Data);
s10(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {CPid, {'Quit'}}, Data);
s10(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec s35(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> 
    {next_state, s35, state_data()} | 
    {next_state, s35, state_data()} | 
    {keep_state, state_data()}.
s35(EventType, {CPid, {'Subject'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s35(EventType, {CPid, {'Subject'}}, Data);
s35(EventType, {CPid, {'DataLine'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s35(EventType, {CPid, {'DataLine'}}, Data);
s35(EventType, {CPid, {'EndOfData'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s35(EventType, {CPid, {'EndOfData'}}, Data);
s35(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec s12(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s12(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {CPid, {'Quit'}}, Data);
s12(EventType, {CPid, {'Ehlo'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {CPid, {'Ehlo'}}, Data);
s12(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec s14(EventType :: term(), {atom()}, state_data()) -> {next_state, s18, state_data()}.
s14(EventType, {'250d'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {'250d'}, Data);
s14(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {'250'}, Data).

-spec send_s14_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s14_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec s19(EventType :: term(), {atom()}, state_data()) -> {next_state, s18, state_data()} | {next_state, s21, state_data()}.
s19(EventType, {'535'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {'535'}, Data);
s19(EventType, {'235'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s19(EventType, {'235'}, Data).

-spec s18(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s18(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {CPid, {'Quit'}}, Data);
s18(EventType, {CPid, {'Auth'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {CPid, {'Auth'}}, Data);
s18(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec send_s40_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s40_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec send_s27_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s27_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

s1(EventType, {'220'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {'220'}, Data).

-spec send_s33_354(CPid :: pid(), Data :: state_data()) -> ok.
send_s33_354(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {354}, Counter}).

-spec send_s48_Ack(CPid :: pid(), Data :: state_data()) -> ok.
send_s48_Ack(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Ack'}, Counter}).

-spec s5(EventType :: term(), {atom()} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s5(EventType, {'Timeout'}, #state_data{mc_counter_2 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {'Timeout'}, NewData);
s5(EventType, {CPid, {'Ehlo'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {CPid, {'Ehlo'}}, Data);
s5(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {CPid, {'Quit'}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec s7(EventType :: term(), {atom()}, state_data()) -> {next_state, s10, state_data()}.
s7(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {'250'}, Data);
s7(EventType, {'250d'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {'250d'}, Data).

-spec send_s52_Ack(CPid :: pid(), Data :: state_data()) -> ok.
send_s52_Ack(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Ack'}, Counter}).

-spec send_s43_221(CPid :: pid(), Data :: state_data()) -> ok.
send_s43_221(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {221}, Counter}).

-spec s40(EventType :: term(), {atom()}, state_data()) -> {next_state, s21, state_data()}.
s40(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s40(EventType, {'250'}, Data).

-spec send_s32_Timeout(CPid :: pid(), Data :: state_data()) -> ok.
send_s32_Timeout(CPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(CPid, {self(), {'Timeout'}, Counter}).

-spec s22(EventType :: term(), {atom()}, state_data()) -> {next_state, s26, state_data()} | {next_state, s21, state_data()}.
s22(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {'250'}, Data);
s22(EventType, {'501'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s22(EventType, {'501'}, Data).

-spec s43(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s43(EventType, {'221'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s43(EventType, {'221'}, Data).

-spec s21(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s21(EventType, {CPid, {'Mail'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s21(EventType, {CPid, {'Mail'}}, Data);
s21(EventType, {CPid, {'Quit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s21(EventType, {CPid, {'Quit'}}, Data);
s21(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec s48(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s48(EventType, {'Ack'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s48(EventType, {'Ack'}, Data).

-spec s26(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s26(EventType, {CPid, {'Rcpt'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s26(EventType, {CPid, {'Rcpt'}}, Data);
s26(EventType, {CPid, {'Bogus'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s26(EventType, {CPid, {'Bogus'}}, Data);
s26(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'StartTls'} 
		orelse Msg =:= {'Mail'} 
		orelse Msg =:= {'Subject'} 
		orelse Msg =:= {'Rcpt'} 
		orelse Msg =:= {'Quit'} 
		orelse Msg =:= {'Auth'} 
		orelse Msg =:= {'Ehlo'} 
		orelse Msg =:= {'Bogus'} 
		orelse Msg =:= {'EndOfData'} 
		orelse Msg =:= {'Data'} 
		orelse Msg =:= {'DataLine'} ->
    {keep_state, Data}.

-spec send_s1_220(CPid :: pid(), _Data :: state_data()) -> ok.
send_s1_220(CPid, _Data) ->
    gen_statem:cast(CPid, {self(), {'220'}}).

-spec s27(EventType :: term(), {atom()}, state_data()) -> {next_state, s26, state_data()}.
s27(EventType, {'250'}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s27(EventType, {'250'}, Data).

-spec send_s7_250(CPid :: pid(), Data :: state_data()) -> ok.
send_s7_250(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'250'}, Counter}).

-spec send_s5_Timeout(CPid :: pid(), Data :: state_data()) -> ok.
send_s5_Timeout(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'Timeout'}, Counter}).

-spec send_s22_501(CPid :: pid(), Data :: state_data()) -> ok.
send_s22_501(CPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(CPid, {self(), {'501'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

