-module(gen_carol).
-behaviour(gen_statem).

-export([init/1, callback_mode/0, code_change/4, terminate/3, start_link/2, send_s1_first/3, s1/3, send_s3_second/3, s3/3, send_s7_sum/2, s7/3, send_s7_diff/2, s8/3, send_s9_sum_result/3, s9/3, s11/3, send_s12_diff_result/2, s12/3, send_s5_cancel/2, s5/3]).

-include("carol.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), srv_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s7, state_data()} | {next_state, s7, state_data(), [term()]}.
-callback s5(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s7(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s8, state_data()} | {next_state, s11, state_data()} | {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {keep_state, state_data()}.
-callback s8(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {next_state, s9, state_data(), [{next_event, internal, {sum_result}}]} | {keep_state, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s11(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {next_state, s12, state_data(), [{next_event, internal, {diff_result}}]} | {keep_state, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {second}}]}.
-callback init(Args :: list()) -> {ok, s1, state_data(), [{next_event, internal, {first}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_carol, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "carol_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> {ok, s1, state_data(), [{next_event, internal, {first}}]}.
init({CallbackModule, _Args}) ->
    io:format("carol: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s7, state_data()} | {next_state, s7, state_data(), [term()]}.
s3(EventType, {second}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {second}, Data).

-spec s5(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s5(EventType, {cancel}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {cancel}, Data).

-spec s7(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s8, state_data()} | {next_state, s11, state_data()} | {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {keep_state, state_data()}.
s7(EventType, {sum}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {sum}, NewData);
s7(EventType, {diff}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {diff}, NewData);
s7(EventType, {SrvPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {SrvPid, {timeout}}, Data);
s7(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {diff} 
		orelse Msg =:= {diff_result} 
		orelse Msg =:= {result_sum} 
		orelse Msg =:= {cancel} 
		orelse Msg =:= {sum} 
		orelse Msg =:= {result_diff} 
		orelse Msg =:= {sum_result} 
		orelse Msg =:= {timeout} ->
    {keep_state, Data}.

-spec send_s5_cancel(AlicePid :: pid(), Data :: state_data()) -> ok.
send_s5_cancel(AlicePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AlicePid, {self(), {cancel}, Counter}).

-spec s8(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {next_state, s9, state_data(), [{next_event, internal, {sum_result}}]} | {keep_state, state_data()}.
s8(EventType, {SrvPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {SrvPid, {timeout}}, Data);
s8(EventType, {SrvPid, {result_sum, Result}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {SrvPid, {result_sum, Result}}, Data);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {diff} 
		orelse Msg =:= {diff_result} 
		orelse Msg =:= {result_sum} 
		orelse Msg =:= {cancel} 
		orelse Msg =:= {sum} 
		orelse Msg =:= {result_diff} 
		orelse Msg =:= {sum_result} 
		orelse Msg =:= {timeout} ->
    {keep_state, Data}.

-spec s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(EventType, {sum_result}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {sum_result}, Data).

-spec s11(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {cancel}}]} | {next_state, s12, state_data(), [{next_event, internal, {diff_result}}]} | {keep_state, state_data()}.
s11(EventType, {SrvPid, {timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {SrvPid, {timeout}}, Data);
s11(EventType, {SrvPid, {result_diff, Result}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {SrvPid, {result_diff, Result}}, Data);
s11(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {diff} 
		orelse Msg =:= {diff_result} 
		orelse Msg =:= {result_sum} 
		orelse Msg =:= {cancel} 
		orelse Msg =:= {sum} 
		orelse Msg =:= {result_diff} 
		orelse Msg =:= {sum_result} 
		orelse Msg =:= {timeout} ->
    {keep_state, Data}.

-spec send_s7_sum(SrvPid :: pid(), Data :: state_data()) -> ok.
send_s7_sum(SrvPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SrvPid, {self(), {sum}, Counter}).

-spec s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(EventType, {diff_result}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {diff_result}, Data).

-spec send_s1_first(SrvPid :: pid(), Number :: integer(), Data :: state_data()) -> ok.
send_s1_first(SrvPid, Number, _Data) ->
    gen_statem:cast(SrvPid, {self(), {first, Number}}).

-spec send_s3_second(SrvPid :: pid(), Number :: integer(), Data :: state_data()) -> ok.
send_s3_second(SrvPid, Number, _Data) ->
    gen_statem:cast(SrvPid, {self(), {second, Number}}).

-spec send_s12_diff_result(AlicePid :: pid(), Data :: state_data()) -> ok.
send_s12_diff_result(AlicePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AlicePid, {self(), {diff_result, result}, Counter}).

-spec send_s9_sum_result(AlicePid :: pid(), Result :: term(), Data :: state_data()) -> ok.
send_s9_sum_result(AlicePid, Result, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AlicePid, {self(), {sum_result, Result}, Counter}).

-spec send_s7_diff(SrvPid :: pid(), Data :: state_data()) -> ok.
send_s7_diff(SrvPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SrvPid, {self(), {diff}, Counter}).

-spec s1(EventType :: term(), {atom()}, state_data()) -> {next_state, s3, state_data(), [{next_event, internal, {second}}]}.
s1(EventType, {first}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s1(EventType, {first}, Data).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

