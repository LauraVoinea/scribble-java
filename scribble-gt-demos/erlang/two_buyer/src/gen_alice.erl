-module(gen_alice).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s4_request_title/3, 
	 s4/3, 
	 s5/3, 
	 send_s6_contribution/3, 
	 s6/3, 
	 s9/3
	 ]).

-include("alice.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), seller_pid :: pid() | undefined, bob_pid :: pid() | undefined}.

-callback s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s5, state_data()} | {keep_state, state_data(), [postpone]} | {stop, normal, state_data()}.
-callback s5(term(), {pid(), {atom(), term()}}, state_data()) -> {keep_state, state_data(), [postpone]} | {next_state, s6, state_data(), [{next_event, internal, {contribution}}]} | {keep_state, state_data()} | {stop, normal, state_data()}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {next_state, s9, state_data()}.
-callback s9(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {keep_state, state_data(), [postpone]}.
-callback init(Args :: list()) -> 
	{ok, s4, state_data(), [{next_event, internal, {request_title}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_alice, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "alice_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s4, state_data(), [{next_event, internal, {request_title}}]}.
init({CallbackModule, _Args}) ->
    io:format("alice: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s4(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {next_state, s5, state_data()} |
    {keep_state, state_data(), [postpone]} |
    {stop, normal, state_data()}.
s4(_EventType, {_Pid, {purchase_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[purchase_notification]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {cancel_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[cancel_notification]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[response_timeout]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {price_quote, Price}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[price_quote, Price]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {not_available}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[not_available]]),
    {keep_state, Data, [postpone]};
s4(EventType, {request_title}, #state_data{mc_counter_2 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {request_title}, NewData);
s4(EventType, {SellerPid, {not_available}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {SellerPid, {not_available}}, NewData);
s4(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {purchase_notification} 
		orelse Msg =:= {not_available} 
		orelse Msg =:= {cancel_notification} 
		orelse Msg =:= {response_timeout} ->
    io:format("gen_alice: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s4(_EventType, {_Pid, {price_quote, Price}, _Counter}, Data) ->
    io:format("gen_alice: Garbage collecting event ~p~n", [{price_quote, Price}]),
    {keep_state, Data}.

-spec s5(term(), {pid(), {atom(), term()}}, state_data()) ->
    {keep_state, state_data(), [postpone]} |
    {next_state, s6, state_data(), [{next_event, internal, {contribution}}]} |
    {keep_state, state_data()} |
    {stop, normal, state_data()}.
s5(_EventType, {_Pid, {purchase_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[purchase_notification]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {cancel_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[cancel_notification]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_alice: Postponing event ~p~n", [[response_timeout]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {not_available}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[not_available]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {price_quote, Price}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[price_quote, Price]]),
    {keep_state, Data, [postpone]};
s5(EventType, {SellerPid, {price_quote, Price}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {SellerPid, {price_quote, Price}}, Data);
s5(EventType, {SellerPid, {not_available}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {SellerPid, {not_available}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {purchase_notification} 
		orelse Msg =:= {cancel_notification} ->
    io:format("gen_alice: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s5(_EventType, {_Pid, {price_quote, Price}, _Counter}, Data) ->
    io:format("gen_alice: Garbage collecting event ~p~n", [{price_quote, Price}]),
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) -> {next_state, s9, state_data()}.
s6(EventType, {contribution}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {contribution}, Data).

-spec s9(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {keep_state, state_data(), [postpone]}.
s9(_EventType, {_Pid, {purchase_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[purchase_notification]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {cancel_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[cancel_notification]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC ->
    io:format("gen_alice: Postponing event ~p~n", [[response_timeout]]),
    {keep_state, Data, [postpone]};
s9(EventType, {BobPid, {purchase_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {BobPid, {purchase_notification}}, NewData);
s9(EventType, {BobPid, {cancel_notification}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {BobPid, {cancel_notification}}, NewData);
s9(EventType, {SellerPid, {response_timeout}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {SellerPid, {response_timeout}}, NewData);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {purchase_notification} 
		orelse Msg =:= {cancel_notification} ->
    io:format("gen_alice: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec send_s6_contribution(BobPid :: pid(), Amount :: term(), Data :: state_data()) -> ok.
send_s6_contribution(BobPid, Amount, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(BobPid, {self(), {contribution, Amount}, Counter}).

-spec send_s4_request_title(SellerPid :: pid(), Title :: term(), Data :: state_data()) -> ok.
send_s4_request_title(SellerPid, Title, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(SellerPid, {self(), {request_title, Title}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

