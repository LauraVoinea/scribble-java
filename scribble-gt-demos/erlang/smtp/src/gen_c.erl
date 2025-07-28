-module(gen_c).
-behaviour(gen_statem).

-export([init/1,
  callback_mode/0,
  code_change/4,
  terminate/3,
  start_link/2,
  s1/3,
  send_s5_Ehlo/2,
  s5/3,
  send_s5_QuitCommit/2,
  s6/3,
  s8/3,
  send_s11_StartTls/2,
  s11/3,
  send_s11_Quit/2,
  s12/3,
  send_s13_Quit/2,
  s13/3,
  send_s13_Ehlo1/2,
  s15/3,
  send_s19_Auth/2,
  s19/3,
  send_s19_Quit/2,
  s20/3,
  send_s22_Mail/2,
  s22/3,
  send_s22_Quit/2,
  s23/3,
  send_s27_Bogus/2,
  s27/3,
  send_s27_Rcpt/2,
  s28/3,
  send_s33_Data/2,
  s33/3,
  s34/3,
  send_s36_DataLine/2,
  s36/3,
  send_s36_EndOfData/2,
  send_s36_Subject/2,
  s41/3,
  s44/3,
  s49/3,
  s51/3,
  s53/3
]).

-include("c.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), s_pid :: pid() | undefined}.

-callback s51(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s53(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s33(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s34, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data()} | {next_state, s51, state_data()}.
-callback s13(EventType :: term(), {atom()}, state_data()) -> {next_state, s49, state_data()} | {next_state, s15, state_data()}.
-callback s34(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()} | {next_state, s36, state_data(), [{next_event, internal, {'EndOfData'}}]} | {next_state, s36, state_data(), [{next_event, internal, {'DataLine'}}]} | {next_state, s36, state_data(), [{next_event, internal, {'Subject'}}]} | {keep_state, state_data()}.
-callback s12(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s13, state_data(), [{next_event, internal, {'Quit'}}]} | {next_state, s13, state_data(), [{next_event, internal, {'Ehlo1'}}]} | {keep_state, state_data()}.
-callback s15(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s15, state_data()} | {next_state, s19, state_data(), [{next_event, internal, {'Auth'}}]} | {next_state, s19, state_data(), [{next_event, internal, {'Quit'}}]} | {keep_state, state_data()}.
-callback s36(EventType :: term(), {atom()}, state_data()) -> {next_state, s36, state_data(), [{next_event, internal, {'EndOfData'}}]} | {next_state, s36, state_data(), [{next_event, internal, {'DataLine'}}]} | {next_state, s36, state_data(), [{next_event, internal, {'Subject'}}]} | {keep_state, state_data()} | {next_state, s41, state_data()}.
-callback s19(EventType :: term(), {atom()}, state_data()) -> {next_state, s20, state_data()} | {stop, normal, state_data()}.
-callback s1(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s5, state_data(), [{next_event, internal, {'Ehlo'}}]} | {next_state, s5, state_data(), [{next_event, internal, {'QuitCommit'}}]} | {keep_state, state_data()}.
-callback s5(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s6, state_data()} | {next_state, s53, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()} | {next_state, s8, state_data()}.
-callback s8(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s11, state_data(), [{next_event, internal, {'StartTls'}}]} | {next_state, s11, state_data(), [{next_event, internal, {'Quit'}}]} | {keep_state, state_data()} | {next_state, s15, state_data()}.
-callback s20(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s19, state_data(), [{next_event, internal, {'Auth'}}]} | {next_state, s19, state_data(), [{next_event, internal, {'Quit'}}]} | {keep_state, state_data()} | {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} | {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]}.
-callback s41(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} | {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]} | {keep_state, state_data()}.
-callback s44(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s22(EventType :: term(), {atom()}, state_data()) -> {next_state, s23, state_data()} | {next_state, s44, state_data()}.
-callback s23(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s27, state_data(), [{next_event, internal, {'Bogus'}}]} | {next_state, s27, state_data(), [{next_event, internal, {'Rcpt'}}]} | {keep_state, state_data()} | {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} | {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]}.
-callback s28(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s27, state_data(), [{next_event, internal, {'Bogus'}}]} | {next_state, s27, state_data(), [{next_event, internal, {'Rcpt'}}]} | {keep_state, state_data()}.
-callback s49(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s27(EventType :: term(), {atom()}, state_data()) -> {next_state, s33, state_data(), [{next_event, internal, {'Data'}}]} | {keep_state, state_data()} | {next_state, s28, state_data()}.
-callback init(Args :: list()) ->
  {ok, s1, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
  {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
  case code:ensure_loaded(CallbackModule) of
    {module, CallbackModule} ->
      gen_statem:start_link({local, CallbackModule}, gen_c, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "c_debug.log"}]}]);
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

-spec send_s19_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s19_Quit(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec send_s13_Ehlo1(SPid :: pid(), Data :: state_data()) -> ok.
send_s13_Ehlo1(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Ehlo1'}, Counter}).

-spec send_s22_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s22_Quit(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec s51(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()}.
s51(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s51(EventType, {SPid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s51(EventType, {SPid, {'Ack'}}, Data);
s51(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Ack'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s53(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()}.
s53(_EventType, {_Pid, {'AckCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['AckCommit']]),
  {keep_state, Data, [postpone]};
s53(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s53(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s53(EventType, {SPid, {'Timeout'}}, Data);
s53(EventType, {SPid, {'AckCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s53(EventType, {SPid, {'AckCommit'}}, Data);
s53(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'250d'}
  orelse Msg =:= {'220'}
  orelse Msg =:= {'EhloCommit'}
  orelse Msg =:= {'Ack'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec send_s36_Subject(SPid :: pid(), Data :: state_data()) -> ok.
send_s36_Subject(SPid, Data) ->
  Counter = Data#state_data.mc_counter_1,
  gen_statem:cast(SPid, {self(), {'Subject'}, Counter}).

-spec s33(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
  {next_state, s34, state_data()} |
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()}.
s33(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s33(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s33(EventType, {'Data'}, #state_data{mc_counter_1 = MC} = Data) ->
  NewData = Data#state_data{mc_counter_1 = MC + 1},
  CallbackModule = get(callback_module),
  CallbackModule:s33(EventType, {'Data'}, NewData);
s33(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
  NewData = Data#state_data{mc_counter_1 = MC + 1},
  CallbackModule = get(callback_module),
  CallbackModule:s33(EventType, {SPid, {'Timeout'}}, NewData);
s33(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s11(EventType :: term(), {atom()}, state_data()) ->
  {next_state, s12, state_data()} |
  {next_state, s51, state_data()}.
s11(EventType, {'StartTls'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s11(EventType, {'StartTls'}, Data);
s11(EventType, {'Quit'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s11(EventType, {'Quit'}, Data).

-spec s13(EventType :: term(), {atom()}, state_data()) ->
  {next_state, s49, state_data()} |
  {next_state, s15, state_data()}.
s13(EventType, {'Quit'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s13(EventType, {'Quit'}, Data);
s13(EventType, {'Ehlo1'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s13(EventType, {'Ehlo1'}, Data).

-spec s34(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()} |
  {next_state, s36, state_data(), [{next_event, internal, {'EndOfData'}}]} |
  {next_state, s36, state_data(), [{next_event, internal, {'DataLine'}}]} |
  {next_state, s36, state_data(), [{next_event, internal, {'Subject'}}]} |
  {keep_state, state_data()}.
s34(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s34(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s34(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s34(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s34(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s34(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s34(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s34(EventType, {SPid, {'Timeout'}}, Data);
s34(EventType, {SPid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s34(EventType, {SPid, {'354'}}, Data);
s34(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'354'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s12(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s13, state_data(), [{next_event, internal, {'Quit'}}]} |
  {next_state, s13, state_data(), [{next_event, internal, {'Ehlo1'}}]} |
  {keep_state, state_data()}.
s12(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250d']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s12(_EventType, {_Pid, {'220'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['220']]),
  {keep_state, Data, [postpone]};
s12(EventType, {SPid, {'220'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s12(EventType, {SPid, {'220'}}, Data);
s12(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'250d'}
  orelse Msg =:= {'220'}
  orelse Msg =:= {'Ack'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s15(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s15, state_data()} |
  {next_state, s19, state_data(), [{next_event, internal, {'Auth'}}]} |
  {next_state, s19, state_data(), [{next_event, internal, {'Quit'}}]} |
  {keep_state, state_data()}.
s15(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250d']]),
  {keep_state, Data, [postpone]};
s15(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s15(EventType, {SPid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s15(EventType, {SPid, {'250d'}}, Data);
s15(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s15(EventType, {SPid, {'250'}}, Data);
s15(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'250d'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec send_s5_Ehlo(SPid :: pid(), Data :: state_data()) -> ok.
send_s5_Ehlo(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Ehlo'}, Counter}).

-spec s36(EventType :: term(), {atom()}, state_data()) ->
  {next_state, s36, state_data(), [{next_event, internal, {'EndOfData'}}]} |
  {next_state, s36, state_data(), [{next_event, internal, {'DataLine'}}]} |
  {next_state, s36, state_data(), [{next_event, internal, {'Subject'}}]} |
  {keep_state, state_data()} |
  {next_state, s41, state_data()}.
s36(EventType, {'DataLine'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s36(EventType, {'DataLine'}, Data);
s36(EventType, {'EndOfData'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s36(EventType, {'EndOfData'}, Data);
s36(EventType, {'Subject'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s36(EventType, {'Subject'}, Data).

-spec send_s5_QuitCommit(SPid :: pid(), Data :: state_data()) -> ok.
send_s5_QuitCommit(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'QuitCommit'}, Counter}).

-spec send_s27_Bogus(SPid :: pid(), Data :: state_data()) -> ok.
send_s27_Bogus(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Bogus'}, Counter}).

-spec s19(EventType :: term(), {atom()}, state_data()) ->
  {next_state, s20, state_data()} |
  {stop, normal, state_data()}.
s19(EventType, {'Auth'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s19(EventType, {'Auth'}, Data);
s19(EventType, {'Quit'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s19(EventType, {'Quit'}, Data).

-spec send_s13_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s13_Quit(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec s1(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s5, state_data(), [{next_event, internal, {'Ehlo'}}]} |
  {next_state, s5, state_data(), [{next_event, internal, {'QuitCommit'}}]} |
  {keep_state, state_data()}.
s1(_EventType, {_Pid, {'AckCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['AckCommit']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'EhloCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['EhloCommit']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250d']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s1(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s1(EventType, {SPid, {'220'}}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s1(EventType, {SPid, {'220'}}, Data).

-spec send_s11_StartTls(SPid :: pid(), Data :: state_data()) -> ok.
send_s11_StartTls(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'StartTls'}, Counter}).

-spec send_s36_DataLine(SPid :: pid(), Data :: state_data()) -> ok.
send_s36_DataLine(SPid, Data) ->
  Counter = Data#state_data.mc_counter_1,
  gen_statem:cast(SPid, {self(), {'DataLine'}, Counter}).

-spec send_s19_Auth(SPid :: pid(), Data :: state_data()) -> ok.
send_s19_Auth(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Auth'}, Counter}).

-spec s5(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
  {next_state, s6, state_data()} |
  {next_state, s53, state_data()} |
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()}.
s5(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'220'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['220']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'EhloCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['EhloCommit']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'AckCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['AckCommit']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['250d']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC + 1 ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s5(EventType, {'Ehlo'}, #state_data{mc_counter_2 = MC} = Data) ->
  NewData = Data#state_data{mc_counter_2 = MC + 1},
  CallbackModule = get(callback_module),
  CallbackModule:s5(EventType, {'Ehlo'}, NewData);
s5(EventType, {'QuitCommit'}, #state_data{mc_counter_2 = MC} = Data) ->
  NewData = Data#state_data{mc_counter_2 = MC + 1},
  CallbackModule = get(callback_module),
  CallbackModule:s5(EventType, {'QuitCommit'}, NewData);
s5(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC + 1 ->
  NewData = Data#state_data{mc_counter_2 = MC + 1},
  CallbackModule = get(callback_module),
  CallbackModule:s5(EventType, {SPid, {'Timeout'}}, NewData);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'250d'}
  orelse Msg =:= {'220'}
  orelse Msg =:= {'Ack'}
  orelse Msg =:= {'EhloCommit'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'Timeout'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s6(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()} |
  {next_state, s8, state_data()}.
s6(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'220'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['220']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250d']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'EhloCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['EhloCommit']]),
  {keep_state, Data, [postpone]};
s6(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s6(EventType, {SPid, {'Timeout'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s6(EventType, {SPid, {'Timeout'}}, Data);
s6(EventType, {SPid, {'EhloCommit'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s6(EventType, {SPid, {'EhloCommit'}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'250d'}
  orelse Msg =:= {'220'}
  orelse Msg =:= {'EhloCommit'}
  orelse Msg =:= {'Ack'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s8(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s11, state_data(), [{next_event, internal, {'StartTls'}}]} |
  {next_state, s11, state_data(), [{next_event, internal, {'Quit'}}]} |
  {keep_state, state_data()} |
  {next_state, s15, state_data()}.
s8(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'220'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['220']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250d']]),
  {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s8(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s8(EventType, {SPid, {'250'}}, Data);
s8(EventType, {SPid, {'250d'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s8(EventType, {SPid, {'250d'}}, Data);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'250d'}
  orelse Msg =:= {'220'}
  orelse Msg =:= {'Ack'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec send_s22_Mail(SPid :: pid(), Data :: state_data()) -> ok.
send_s22_Mail(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Mail'}, Counter}).

-spec s20(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s19, state_data(), [{next_event, internal, {'Auth'}}]} |
  {next_state, s19, state_data(), [{next_event, internal, {'Quit'}}]} |
  {keep_state, state_data()} |
  {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} |
  {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]}.
s20(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['535']]),
  {keep_state, Data, [postpone]};
s20(_EventType, {_Pid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['235']]),
  {keep_state, Data, [postpone]};
s20(EventType, {SPid, {'535'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s20(EventType, {SPid, {'535'}}, Data);
s20(EventType, {SPid, {'235'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s20(EventType, {SPid, {'235'}}, Data);
s20(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'535'}
  orelse Msg =:= {'235'}
  orelse Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s41(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} |
  {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]} |
  {keep_state, state_data()}.
s41(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s41(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s41(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s41(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s41(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s41(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s41(EventType, {SPid, {'250'}}, Data);
s41(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec send_s11_Quit(SPid :: pid(), Data :: state_data()) -> ok.
send_s11_Quit(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Quit'}, Counter}).

-spec s44(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()}.
s44(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s44(EventType, {SPid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s44(EventType, {SPid, {'221'}}, Data);
s44(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'221'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s22(EventType :: term(), {atom()}, state_data()) ->
  {next_state, s23, state_data()} |
  {next_state, s44, state_data()}.
s22(EventType, {'Mail'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s22(EventType, {'Mail'}, Data);
s22(EventType, {'Quit'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s22(EventType, {'Quit'}, Data).

-spec send_s27_Rcpt(SPid :: pid(), Data :: state_data()) -> ok.
send_s27_Rcpt(SPid, Data) ->
  Counter = Data#state_data.mc_counter_2,
  gen_statem:cast(SPid, {self(), {'Rcpt'}, Counter}).

-spec s23(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s27, state_data(), [{next_event, internal, {'Bogus'}}]} |
  {next_state, s27, state_data(), [{next_event, internal, {'Rcpt'}}]} |
  {keep_state, state_data()} |
  {next_state, s22, state_data(), [{next_event, internal, {'Mail'}}]} |
  {next_state, s22, state_data(), [{next_event, internal, {'Quit'}}]}.
s23(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s23(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s23(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s23(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s23(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s23(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s23(EventType, {SPid, {'250'}}, Data);
s23(EventType, {SPid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s23(EventType, {SPid, {'501'}}, Data);
s23(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec send_s36_EndOfData(SPid :: pid(), Data :: state_data()) -> ok.
send_s36_EndOfData(SPid, Data) ->
  Counter = Data#state_data.mc_counter_1,
  gen_statem:cast(SPid, {self(), {'EndOfData'}, Counter}).

-spec s28(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {next_state, s27, state_data(), [{next_event, internal, {'Bogus'}}]} |
  {next_state, s27, state_data(), [{next_event, internal, {'Rcpt'}}]} |
  {keep_state, state_data()}.
s28(_EventType, {_Pid, {'Timeout'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['Timeout']]),
  {keep_state, Data, [postpone]};
s28(_EventType, {_Pid, {'221'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['221']]),
  {keep_state, Data, [postpone]};
s28(_EventType, {_Pid, {'501'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['501']]),
  {keep_state, Data, [postpone]};
s28(_EventType, {_Pid, {'354'}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
  io:format("gen_c: Postponing event ~p~n", [['354']]),
  {keep_state, Data, [postpone]};
s28(_EventType, {_Pid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['250']]),
  {keep_state, Data, [postpone]};
s28(EventType, {SPid, {'250'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s28(EventType, {SPid, {'250'}}, Data);
s28(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'221'}
  orelse Msg =:= {'354'}
  orelse Msg =:= {'250'}
  orelse Msg =:= {'501'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s49(term(), {pid(), {atom(), term()}}, state_data()) ->
  {keep_state, state_data(), [postpone]} |
  {stop, normal, state_data()}.
s49(_EventType, {_Pid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
  io:format("gen_c: Postponing event ~p~n", [['Ack']]),
  {keep_state, Data, [postpone]};
s49(EventType, {SPid, {'Ack'}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
  CallbackModule = get(callback_module),
  CallbackModule:s49(EventType, {SPid, {'Ack'}}, Data);
s49(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {'Ack'}
  orelse Msg =:= {'AckCommit'} ->
  io:format("gen_c: Garbage collecting event ~p~n", [Msg]),
  {keep_state, Data}.

-spec s27(EventType :: term(), {atom()}, state_data()) ->
  {next_state, s33, state_data(), [{next_event, internal, {'Data'}}]} |
  {keep_state, state_data()} |
  {next_state, s28, state_data()}.
s27(EventType, {'Bogus'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s27(EventType, {'Bogus'}, Data);
s27(EventType, {'Rcpt'}, Data) ->
  CallbackModule = get(callback_module),
  CallbackModule:s27(EventType, {'Rcpt'}, Data).

-spec send_s33_Data(SPid :: pid(), Data :: state_data()) -> ok.
send_s33_Data(SPid, Data) ->
  Counter = Data#state_data.mc_counter_1,
  gen_statem:cast(SPid, {self(), {'Data'}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
  {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
  {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
  ok.
