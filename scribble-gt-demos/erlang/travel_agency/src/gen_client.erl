-module(gen_client).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 send_s3_booking_request/3, 
	 s3/3, 
	 s4/3, 
	 send_s9_accept_offer/2, 
	 s9/3, 
	 send_s9_resubmit_request/2, 
	 send_s9_reject_offer/2, 
	 s10/3, 
	 send_s11_provide_address/3, 
	 s11/3, 
	 s12/3, 
	 s14/3, 
	 send_s15_cancel_booking/2, 
	 s15/3, 
	 s17/3, 
	 send_s18_resubmitting/2, 
	 s18/3, 
	 send_s6_cancel_agency/2, 
	 s6/3, 
	 send_s7_cancel_supplier/2, 
	 s7/3
	 ]).

-include("client.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), agency_pid :: pid() | undefined, supplier_pid :: pid() | undefined}.

-callback s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
-callback s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s9(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s10, state_data()} | {next_state, s17, state_data()} | {next_state, s14, state_data()}.
-callback s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data()}.
-callback s12(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
-callback s15(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback s18(EventType :: term(), {atom()}, state_data()) -> {ok, s3, state_data(), [{next_event, internal, {booking_request}}]}.
-callback init(Args :: list()) -> 
	{ok, s3, state_data(), [{next_event, internal, {booking_request}}]}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_client, {CallbackModule, Args}, []);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s3, state_data(), [{next_event, internal, {booking_request}}]}.
init({CallbackModule, _Args}) ->
    io:format("client: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s3(EventType :: term(), {atom()}, state_data()) -> {next_state, s4, state_data()}.
s3(EventType, {booking_request}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s3(EventType, {booking_request}, Data).

s4(_EventType, {_Pid, {price_adjustment, Price}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[price_adjustment, Price]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {repeat_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[repeat_confirmation]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {reject_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[reject_confirmation]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {accept_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[accept_confirmation]]),
    {keep_state, Data, [postpone]};
s4(_EventType, {_Pid, {confirm_date, Date}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[confirm_date, Date]]),
    {keep_state, Data, [postpone]};
s4(EventType, {AgencyPid, {price_quote, Price}}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s4(EventType, {AgencyPid, {price_quote, Price}}, Data).

s6(EventType, {cancel_agency}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {cancel_agency}, Data).

-spec s7(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(EventType, {cancel_supplier}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s7(EventType, {cancel_supplier}, Data).

-spec send_s18_resubmitting(SupplierPid :: pid(), Data :: state_data()) -> ok.
send_s18_resubmitting(SupplierPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SupplierPid, {self(), {resubmitting}, Counter}).

-spec send_s15_cancel_booking(SupplierPid :: pid(), Data :: state_data()) -> ok.
send_s15_cancel_booking(SupplierPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SupplierPid, {self(), {cancel_booking}, Counter}).

-spec s9(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {next_state, s10, state_data()} | {next_state, s17, state_data()} | {next_state, s14, state_data()}.
s9(_EventType, {_Pid, {repeat_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[repeat_confirmation]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {reject_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[reject_confirmation]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {accept_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[accept_confirmation]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {confirm_date, Date}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[confirm_date, Date]]),
    {keep_state, Data, [postpone]};
s9(_EventType, {_Pid, {price_quote, Price}}, Data) ->
    io:format("gen_client: Postponing event ~p~n", [[price_quote, Price]]),
    {keep_state, Data, [postpone]};
s9(EventType, {accept_offer}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {accept_offer}, NewData);
s9(EventType, {resubmit_request}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {resubmit_request}, NewData);
s9(EventType, {reject_offer}, #state_data{mc_counter_1 = MC} = Data) ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {reject_offer}, NewData);
s9(EventType, {AgencyPid, {price_adjustment, Price}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s9(EventType, {AgencyPid, {price_adjustment, Price}}, NewData);
s9(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {accept_confirmation} 
		orelse Msg =:= {repeat_confirmation} 
		orelse Msg =:= {reject_confirmation} ->
    io:format("gen_client: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s9(_EventType, {_Pid, {confirm_date, Date}, _Counter}, Data) ->
    io:format("gen_client: Garbage collecting event ~p~n", [{confirm_date, Date}]),
    {keep_state, Data};
s9(_EventType, {_Pid, {price_adjustment, Price}, _Counter}, Data) ->
    io:format("gen_client: Garbage collecting event ~p~n", [{price_adjustment, Price}]),
    {keep_state, Data}.

-spec send_s9_accept_offer(AgencyPid :: pid(), Data :: state_data()) -> ok.
send_s9_accept_offer(AgencyPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AgencyPid, {self(), {accept_offer}, Counter}).

-spec send_s9_reject_offer(AgencyPid :: pid(), Data :: state_data()) -> ok.
send_s9_reject_offer(AgencyPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AgencyPid, {self(), {reject_offer}, Counter}).

-spec send_s11_provide_address(SupplierPid :: pid(), Address :: term(), Data :: state_data()) -> ok.
send_s11_provide_address(SupplierPid, Address, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SupplierPid, {self(), {provide_address, Address}, Counter}).

-spec s11(EventType :: term(), {atom()}, state_data()) -> {next_state, s12, state_data()}.
s11(EventType, {provide_address}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s11(EventType, {provide_address}, Data).

-spec send_s3_booking_request(AgencyPid :: pid(), Destination :: term(), _Data :: state_data()) -> ok.
send_s3_booking_request(AgencyPid, Destination, _Data) ->
    gen_statem:cast(AgencyPid, {self(), {booking_request, Destination}}).

s10(_EventType, {_Pid, {confirm_date, Date}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[confirm_date, Date]]),
    {keep_state, Data, [postpone]};
s10(EventType, {AgencyPid, {price_adjustment, Price}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {AgencyPid, {price_adjustment, Price}}, Data);
s10(EventType, {AgencyPid, {accept_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s10(EventType, {AgencyPid, {accept_confirmation}}, Data);
s10(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {accept_confirmation} ->
    io:format("gen_client: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s10(_EventType, {_Pid, {confirm_date, Date}, _Counter}, Data) ->
    io:format("gen_client: Garbage collecting event ~p~n", [{confirm_date, Date}]),
    {keep_state, Data}.

-spec send_s7_cancel_supplier(SupplierPid :: pid(), Data :: state_data()) -> ok.
send_s7_cancel_supplier(SupplierPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(SupplierPid, {self(), {cancel_supplier}, Counter}).

-spec s12(term(), {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s12(EventType, {SupplierPid, {confirm_date, Date}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s12(EventType, {SupplierPid, {confirm_date, Date}}, Data);
s12(_EventType, {_Pid, {confirm_date, Date}, _Counter}, Data) ->
    io:format("gen_client: Garbage collecting event ~p~n", [{confirm_date, Date}]),
    {keep_state, Data}.

-spec s15(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s15(EventType, {cancel_booking}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s15(EventType, {cancel_booking}, Data).

s14(EventType, {AgencyPid, {price_adjustment, Price}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {AgencyPid, {price_adjustment, Price}}, Data);
s14(EventType, {AgencyPid, {reject_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s14(EventType, {AgencyPid, {reject_confirmation}}, Data);
s14(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {reject_confirmation} ->
    io:format("gen_client: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data}.

s17(_EventType, {_Pid, {reject_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[reject_confirmation]]),
    {keep_state, Data, [postpone]};
s17(_EventType, {_Pid, {accept_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[accept_confirmation]]),
    {keep_state, Data, [postpone]};
s17(_EventType, {_Pid, {confirm_date, Date}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter >= MC ->
    io:format("gen_client: Postponing event ~p~n", [[confirm_date, Date]]),
    {keep_state, Data, [postpone]};
s17(_EventType, {_Pid, {price_quote, Price}}, Data) ->
    io:format("gen_client: Postponing event ~p~n", [[price_quote, Price]]),
    {keep_state, Data, [postpone]};
s17(EventType, {AgencyPid, {repeat_confirmation}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s17(EventType, {AgencyPid, {repeat_confirmation}}, Data);
s17(EventType, {AgencyPid, {price_adjustment, Price}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC ->
    CallbackModule = get(callback_module),
    CallbackModule:s17(EventType, {AgencyPid, {price_adjustment, Price}}, Data);
s17(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {accept_confirmation} 
		orelse Msg =:= {repeat_confirmation} 
		orelse Msg =:= {reject_confirmation} ->
    io:format("gen_client: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s17(_EventType, {_Pid, {confirm_date, Date}, _Counter}, Data) ->
    io:format("gen_client: Garbage collecting event ~p~n", [{confirm_date, Date}]),
    {keep_state, Data}.

-spec send_s6_cancel_agency(AgencyPid :: pid(), Data :: state_data()) -> ok.
send_s6_cancel_agency(AgencyPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AgencyPid, {self(), {cancel_agency}, Counter}).

-spec s18(EventType :: term(), {atom()}, state_data()) -> {ok, s3, state_data(), [{next_event, internal, {booking_request}}]}.
s18(EventType, {resubmitting}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s18(EventType, {resubmitting}, Data).

-spec send_s9_resubmit_request(AgencyPid :: pid(), Data :: state_data()) -> ok.
send_s9_resubmit_request(AgencyPid, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(AgencyPid, {self(), {resubmit_request}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

