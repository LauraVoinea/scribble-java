-module(gen_bob).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s4/3, 
	 s5/3, 
	 send_s8_reject_quote/2, 
	 s8/3, 
	 send_s8_accept_quote/2, 
	 s9/3, 
	 send_s10_purchase_notification/2, 
	 s10/3, 
	 s12/3, 
	 send_s13_cancel_notification/2, 
	 s13/3
	 ]).

-include("bob.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), seller_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s5(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
-callback s10(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s13(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s12(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s8(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s12, state_data()} | {next_state, s9, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
-callback s9(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_bob, {CallbackModule, Args}, 
            % []);
            [{debug, [trace, {log_to_file, "bob_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("bob: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s5, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s4(EventType, {SellerPid, {price_quote, Price}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {SellerPid, {price_quote, Price}}, NewData);
s4(EventType, {SellerPid, {not_available}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {SellerPid, {not_available}}, NewData);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {not_available} ->
    {keep_state, Data}.

-spec s5(EventType :: term(), term(), state_data()) -> {keep_state, state_data()}.
s5(EventType, {AlicePid, {contribution, Amount}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {AlicePid, {contribution, Amount}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {not_available} ->
    {keep_state, Data}.

-spec s10(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s10(EventType, {purchase_notification}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {purchase_notification}, Data).

-spec send_s8_reject_quote(SellerPid :: pid(), Data :: state_data()) -> ok.
send_s8_reject_quote(SellerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SellerPid, {self(), {reject_quote}, Counter}).

-spec s13(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s13(EventType, {cancel_notification}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {cancel_notification}, Data).

-spec send_s13_cancel_notification(AlicePid :: pid(), Data :: state_data()) -> ok.
send_s13_cancel_notification(AlicePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AlicePid, {self(), {cancel_notification}, Counter}).

-spec s12(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s12(EventType, {SellerPid, {cancel_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {SellerPid, {cancel_confirmation}}, Data);
s12(EventType, {SellerPid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {SellerPid, {response_timeout}}, Data);
s12(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {cancel_confirmation} 
		orelse Msg =:= {purchase_confirmed} 
		orelse Msg =:= {response_timeout} 
		orelse Msg =:= {not_available} ->
    {keep_state, Data};
s12(_EventType, {_Pid, {contribution, Amount}, _Counter}, Data) ->
    {keep_state, Data};
s12(_EventType, {_Pid, {price_quote, Price}, _Counter}, Data) ->
    {keep_state, Data}.

-spec s8(EventType :: term(), {pid(), {term()}, integer()} | term(), state_data()) -> {next_state, s12, state_data()} | {next_state, s9, state_data()} | {stop, normal, state_data()} | {keep_state, state_data()}.
s8(EventType, {reject_quote}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {reject_quote}, NewData);
s8(EventType, {accept_quote}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {accept_quote}, NewData);
s8(EventType, {SellerPid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {SellerPid, {response_timeout}}, Data);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {cancel_confirmation} 
		orelse Msg =:= {purchase_confirmed} 
		orelse Msg =:= {response_timeout} 
		orelse Msg =:= {not_available} ->
    {keep_state, Data};
s8(_EventType, {_Pid, {contribution, Amount}, _Counter}, Data) ->
    {keep_state, Data};
s8(_EventType, {_Pid, {price_quote, Price}, _Counter}, Data) ->
    {keep_state, Data}.

-spec s9(term() | EventType :: term(), {pid(), {atom(), term()}} | term(), state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s9(EventType, {SellerPid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {SellerPid, {response_timeout}}, Data);
s9(EventType, {SellerPid, {purchase_confirmed}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {SellerPid, {purchase_confirmed}}, Data);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {cancel_confirmation} 
		orelse Msg =:= {purchase_confirmed} 
		orelse Msg =:= {response_timeout} 
		orelse Msg =:= {not_available} ->
    {keep_state, Data};
s9(_EventType, {_Pid, {contribution, Amount}, _Counter}, Data) ->
    {keep_state, Data};
s9(_EventType, {_Pid, {price_quote, Price}, _Counter}, Data) ->
    {keep_state, Data}.

-spec send_s10_purchase_notification(AlicePid :: pid(), Data :: state_data()) -> ok.
send_s10_purchase_notification(AlicePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AlicePid, {self(), {purchase_notification}, Counter}).

-spec send_s8_accept_quote(SellerPid :: pid(), Data :: state_data()) -> ok.
send_s8_accept_quote(SellerPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SellerPid, {self(), {accept_quote}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

