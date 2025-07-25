-module(gen_seller).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s5_not_available/2, 
	 s5/3, 
	 send_s6_price_quote/3, 
	 s6/3, 
	 send_s7_price_quote/3, 
	 s7/3, 
	 send_s11_response_timeout/2, 
	 s11/3, 
	 send_s12_purchase_confirmed/2, 
	 s12/3, 
	 send_s14_cancel_confirmation/2, 
	 s14/3, 
	 send_s9_response_timeout/2, 
	 s9/3, 
	 send_s3_not_available/2, 
	 s3/3
	 ]).

-include("seller.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), bob_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s14(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s5, state_data(), [{next_event, internal, {not_available}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_seller, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s5, state_data(), [{next_event, internal, {not_available}}]}.
init({CallbackModule, _Args}) ->
    io:format("seller: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(EventType, {not_available}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {not_available}, Data).

-spec send_s11_response_timeout(BobPid :: pid(), Data :: state_data()) -> ok.
send_s11_response_timeout(BobPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BobPid, {self(), {response_timeout}, Counter}).

s5(EventType, {not_available}, #state_data{mc_counter_2 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_2 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {not_available}, NewData);
s5(_EventType, {_Pid, {reject_quote}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_seller: Postponing event ~p~n", [[reject_quote]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {accept_quote}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_seller: Postponing event ~p~n", [[accept_quote]]),
    {keep_state, Data, [postpone]};
s5(EventType, {AlicePid, {request_title, Title}, Counter}, #state_data{mc_counter_2 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {AlicePid, {request_title, Title}}, Data);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {reject_quote} 
		orelse Msg =:= {accept_quote} ->
    io:format("gen_seller: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s5(_EventType, {_Pid, {request_title, Title}, _Counter}, Data) ->
    io:format("gen_seller: Garbage collecting event ~p~n", [{request_title, Title}]),
    {keep_state, Data}.

s6(EventType, {price_quote}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {price_quote}, Data).

s7(EventType, {price_quote}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {price_quote}, Data).

-spec s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(EventType, {response_timeout}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {response_timeout}, Data).

s11(EventType, {response_timeout}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {response_timeout}, NewData);
s11(EventType, {BobPid, {accept_quote}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {BobPid, {accept_quote}}, Data);
s11(EventType, {BobPid, {reject_quote}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {BobPid, {reject_quote}}, Data);
s11(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {reject_quote} 
		orelse Msg =:= {accept_quote} ->
    io:format("gen_seller: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s11(_EventType, {_Pid, {request_title, Title}, _Counter}, Data) ->
    io:format("gen_seller: Garbage collecting event ~p~n", [{request_title, Title}]),
    {keep_state, Data}.

-spec send_s5_not_available(AlicePid :: pid(), Data :: state_data()) -> ok.
send_s5_not_available(AlicePid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(AlicePid, {self(), {not_available}, Counter}).

-spec s12(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(EventType, {purchase_confirmed}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {purchase_confirmed}, Data).

-spec send_s6_price_quote(AlicePid :: pid(), Price :: term(), Data :: state_data()) -> ok.
send_s6_price_quote(AlicePid, Price, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(AlicePid, {self(), {price_quote, Price}, Counter}).

-spec s14(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s14(EventType, {cancel_confirmation}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {cancel_confirmation}, Data).

-spec send_s3_not_available(BobPid :: pid(), Data :: state_data()) -> ok.
send_s3_not_available(BobPid, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(BobPid, {self(), {not_available}, Counter}).

-spec send_s12_purchase_confirmed(BobPid :: pid(), Data :: state_data()) -> ok.
send_s12_purchase_confirmed(BobPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BobPid, {self(), {purchase_confirmed}, Counter}).

-spec send_s7_price_quote(BobPid :: pid(), Price :: term(), Data :: state_data()) -> ok.
send_s7_price_quote(BobPid, Price, Data) ->
    Counter = Data#state_data.mc_counter_2,
    gen_statem:cast(BobPid, {self(), {price_quote, Price}, Counter}).

-spec send_s14_cancel_confirmation(BobPid :: pid(), Data :: state_data()) -> ok.
send_s14_cancel_confirmation(BobPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(BobPid, {self(), {cancel_confirmation}, Counter}).

-spec send_s9_response_timeout(AlicePid :: pid(), Data :: state_data()) -> ok.
send_s9_response_timeout(AlicePid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AlicePid, {self(), {response_timeout}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

