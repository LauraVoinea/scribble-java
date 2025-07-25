-module(client).
-behaviour(gen_client).

-export([
    init/1, 
    callback_mode/0, 
    start_link/0, 
    s3/3, 
    s4/3, 
    make_choice_s9/1, 
    s9/3, 
    s10/3, 
    s11/3, 
    s12/3, 
    s14/3, 
    s15/3, 
    s17/3, 
    s18/3, 
    s6/3, 
    s7/3
    ]).

-include("client.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), agency_pid :: pid() | undefined, supplier_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_client:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s3, state_data(), [{next_event, internal, {booking_request}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("client initialized ~n", []),
    {ok, s3, Data, [{next_event, internal, {booking_request}}]}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s4, state_data()}.
s3(internal, {booking_request}, Data) ->
    Data1 = connection(Data),
    Destination = "Paris", % Example destination
    AgencyPid = Data1#state_data.agency_pid,
    io:format("Client: s3 Sending booking_request to Agency for ~p~n", [Destination]),
    gen_client:send_s3_booking_request(AgencyPid, Destination, Data),
    {next_state, s4, Data1}.

-spec s4(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s9, state_data(), [{next_event, internal, {accept_offer}}]} |
    {next_state, s6, state_data(), [{next_event, internal, {cancel_agency}}]} |
    {next_state, s11, state_data(), [{next_event, internal, {provide_address}}]}.
s4(cast, {AgencyPid, {price_quote, Price}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s4 Received price_quote from Agency ~p with Price ~p~n", [AgencyPid, Price]),
    case make_choice_s9(Data) of
        1 ->
            io:format("Client: s4 Choosing accept_offer ~n", []),
            {next_state, s9, Data, [{next_event, internal, {accept_offer}}]};
        2 ->
            io:format("Client: s4 Choosing resubmit_request ~n", []),
            {next_state, s9, Data, [{next_event, internal, {resubmit_request}}]};
        3 ->
            io:format("Client: s4 Choosing reject_offer ~n", []),
            {next_state, s9, Data, [{next_event, internal, {reject_offer}}]}
    end.

-spec s11(internal, {atom()}, state_data()) -> {next_state, s12, state_data()}.
s11(internal, {provide_address}, #state_data{supplier_pid = SupplierPid} = Data) ->
    Address = "123 Main St, Paris", % Example address
    io:format("Client: s11 Sending provide_address to Supplier ~p~n", [Address]),
    gen_client:send_s11_provide_address(SupplierPid, Address, Data),
    {next_state, s12, Data}.

-spec s6(internal, {atom()}, state_data()) -> {next_state, s7, state_data(), [{next_event, internal, {cancel_agency}}]}.
s6(internal, {cancel_agency}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s6 Sending cancel_agency to Agency ~n", []),
    gen_client:send_s6_cancel_agency(AgencyPid, Data),
    {next_state, s7, Data, [{next_event, internal, {cancel_supplier}}]}.

-spec s10(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s6, state_data(), [{next_event, internal, {cancel_agency}}]} |
    {next_state, s11, state_data(), [{next_event, internal, {provide_address}}]}.
s10(cast, {AgencyPid, {price_adjustment, Price}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s10 Received price_adjustment from Agency ~p~n", [Price]),
    {next_state, s6, Data, [{next_event, internal, {cancel_agency}}]};
s10(cast, {AgencyPid, {accept_confirmation}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s10 Received accept_confirmation from Agency ~n", []),
    {next_state, s11, Data, [{next_event, internal, {provide_address}}]}.

-spec s7(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s7(internal, {cancel_supplier}, #state_data{supplier_pid = SupplierPid} = Data) ->
    io:format("Client: s7 Sending cancel_supplier to Supplier ~n", []),
    gen_client:send_s7_cancel_supplier(SupplierPid, Data),
    {stop, normal, Data}.

-spec s12(cast, {pid(), {atom(), term()}}, state_data()) -> {stop, normal, state_data()}.
s12(cast, {SupplierPid, {confirm_date, Date}}, #state_data{supplier_pid = SupplierPid} = Data) ->
    io:format("Client: s12 Received confirm_date from Supplier ~p for date ~p~n", [SupplierPid, Date]),
    {stop, normal, Data}.

-spec s15(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s15(internal, {cancel_booking}, #state_data{supplier_pid = SupplierPid} = Data) ->
    io:format("Client: s15 Sending cancel_booking to Supplier ~n", []),
    gen_client:send_s15_cancel_booking(SupplierPid, Data),
    {stop, normal, Data}.

-spec s9(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s10, state_data()} | 
    {next_state, s17, state_data()} | 
    {next_state, s14, state_data()} |
    {next_state, s6, state_data(), [{next_event, internal, {cancel_agency}}]}.
s9(internal, {accept_offer}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s9 Sending accept_offer to Agency ~n", []),
    gen_client:send_s9_accept_offer(AgencyPid, Data),
    {next_state, s10, Data};
s9(internal, {resubmit_request}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s9 Sending resubmit_request to Agency ~n", []),
    gen_client:send_s9_resubmit_request(AgencyPid, Data),
    {next_state, s17, Data};
s9(internal, {reject_offer}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s9 Sending reject_offer to Agency ~n", []),
    gen_client:send_s9_reject_offer(AgencyPid, Data),
    {next_state, s14, Data};
s9(cast, {AgencyPid, {price_adjustment, Price}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s9 Received price_adjustment from Agency ~p with Price ~p~n", [AgencyPid, Price]),
    {next_state, s6, Data, [{next_event, internal, {cancel_agency}}]}.

-spec s14(cast, {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s6, state_data(), [{next_event, internal, {cancel_agency}}]} | 
    {next_state, s15, state_data(), [{next_event, internal, {cancel_booking}}]}.
s14(cast, {AgencyPid, {price_adjustment, Price}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s14 Received price_adjustment from Agency ~p with Price ~p~n", [AgencyPid, Price]),
    {next_state, s6, Data, [{next_event, internal, {cancel_agency}}]};
s14(cast, {AgencyPid, {reject_confirmation}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s14 Received reject_confirmation from Agency ~n", []),
    {next_state, s15, Data, [{next_event, internal, {cancel_booking}}]}.

-spec make_choice_s9(state_data()) -> integer().
make_choice_s9(_Data) ->
    rand:uniform(3).

-spec s17(cast, {pid(), {atom(), term()} | {atom()}}, state_data()) -> 
    {next_state, s18, state_data(), [{next_event, internal, {resubmitting}}]} | 
    {next_state, s6, state_data(), [{next_event, internal, {cancel_agency}}]}.
s17(cast, {AgencyPid, {repeat_confirmation}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s17 Received repeat_confirmation from Agency ~n", []),
    {next_state, s18, Data, [{next_event, internal, {resubmitting}}]};
s17(cast, {AgencyPid, {price_adjustment, Price}}, #state_data{agency_pid = AgencyPid} = Data) ->
    io:format("Client: s17 Received price_adjustment from Agency ~p with Price ~p~n", [AgencyPid, Price]),
    {next_state, s6, Data, [{next_event, internal, {cancel_agency}}]}.

-spec s18(internal, {atom()}, state_data()) -> 
    {next_state, s3, state_data(), [{next_event, internal, {booking_request}}]}.
s18(internal, {resubmitting}, #state_data{supplier_pid = SupplierPid} = Data) ->
    io:format("Client: s18 Sending resubmitting to Supplier ~n", []),
    gen_client:send_s18_resubmitting(SupplierPid, Data),
    {next_state, s3, Data, [{next_event, internal, {booking_request}}]}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("client connected ~n", []),
    AgencyPid = case whereis(agency) of
        undefined ->
            io:format("agency is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(agency);
        Pid_agency ->
            Pid_agency
    end,
    SupplierPid = case whereis(supplier) of
        undefined ->
            io:format("supplier is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(supplier);
        Pid_supplier ->
            Pid_supplier
    end,
    Data#state_data{agency_pid = AgencyPid, supplier_pid = SupplierPid}.

