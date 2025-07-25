-module(gen_agency).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s3/3, 
	 send_s4_price_quote/3, 
	 s4/3, 
	 send_s8_price_adjustment/3, 
	 s8/3, 
	 send_s9_accept_confirmation/2, 
	 s9/3, 
	 send_s11_reject_confirmation/2, 
	 s11/3, 
	 send_s13_repeat_confirmation/2, 
	 s13/3, 
	 s6/3
	 ]).

-include("agency.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), client_pid :: pid() | undefined, supplier_pid :: pid() | undefined}.

-callback s11(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s6(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
-callback s13(EventType :: term(), {atom()}, state_data()) -> {ok, s3, state_data()}.
-callback s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
-callback s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s3, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_agency, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s3, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("agency: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

s3(_EventType, {_Pid, {cancel_agency}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_agency: Postponing event ~p~n", [[cancel_agency]]),
    {keep_state, Data, [postpone]};
s3(_EventType, {_Pid, {resubmit_request}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_agency: Postponing event ~p~n", [[resubmit_request]]),
    {keep_state, Data, [postpone]};
s3(_EventType, {_Pid, {accept_offer}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_agency: Postponing event ~p~n", [[accept_offer]]),
    {keep_state, Data, [postpone]};
s3(_EventType, {_Pid, {reject_offer}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_agency: Postponing event ~p~n", [[reject_offer]]),
    {keep_state, Data, [postpone]};
s3(EventType, {ClientPid, {booking_request, Destination}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {ClientPid, {booking_request, Destination}}, Data).

-spec send_s13_repeat_confirmation(ClientPid :: pid(), Data :: state_data()) -> ok.
send_s13_repeat_confirmation(ClientPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ClientPid, {self(), {repeat_confirmation}, Counter}).

-spec send_s11_reject_confirmation(ClientPid :: pid(), Data :: state_data()) -> ok.
send_s11_reject_confirmation(ClientPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ClientPid, {self(), {reject_confirmation}, Counter}).

s4(EventType, {price_quote}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {price_quote}, Data).

-spec s11(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s11(EventType, {reject_confirmation}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {reject_confirmation}, Data).

-spec s6(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s6(EventType, {ClientPid, {cancel_agency}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {ClientPid, {cancel_agency}}, Data);
s6(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {cancel_agency} ->
    io:format("gen_agency: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s13(EventType :: term(), {atom()}, state_data()) -> {ok, s3, state_data()}.
s13(EventType, {repeat_confirmation}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s13(EventType, {repeat_confirmation}, Data).

-spec send_s9_accept_confirmation(ClientPid :: pid(), Data :: state_data()) -> ok.
send_s9_accept_confirmation(ClientPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ClientPid, {self(), {accept_confirmation}, Counter}).

-spec s8(EventType :: term(), {atom()}, state_data()) -> {next_state, s6, state_data()}.
s8(_EventType, {_Pid, {cancel_agency}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_agency: Postponing event ~p~n", [[cancel_agency]]),
    {keep_state, Data, [postpone]};
s8(_EventType, {_Pid, {booking_request, Destination}}, Data) ->
    io:format("gen_agency: Postponing event ~p~n", [[booking_request, Destination]]),
    {keep_state, Data, [postpone]};
s8(EventType, {price_adjustment}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {price_adjustment}, NewData);
s8(EventType, {ClientPid, {reject_offer}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {ClientPid, {reject_offer}}, Data);
s8(EventType, {ClientPid, {resubmit_request}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {ClientPid, {resubmit_request}}, Data);
s8(EventType, {ClientPid, {accept_offer}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s8(EventType, {ClientPid, {accept_offer}}, Data);
s8(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {reject_offer} 
		orelse Msg =:= {accept_offer} 
		orelse Msg =:= {cancel_agency} 
		orelse Msg =:= {resubmit_request} ->
    io:format("gen_agency: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

-spec s9(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(EventType, {accept_confirmation}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {accept_confirmation}, Data).

-spec send_s8_price_adjustment(ClientPid :: pid(), Price :: term(), Data :: state_data()) -> ok.
send_s8_price_adjustment(ClientPid, Price, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ClientPid, {self(), {price_adjustment, Price}, Counter}).

-spec send_s4_price_quote(ClientPid :: pid(), Price :: term(), _Data :: state_data()) -> ok.
send_s4_price_quote(ClientPid, Price, _Data) ->
    gen_statem:cast(ClientPid, {self(), {price_quote, Price}}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

