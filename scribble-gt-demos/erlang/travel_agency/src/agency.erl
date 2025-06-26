-module(agency).
-behaviour(gen_agency).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s3/3,
	 s4/3,
	 make_choice_price_adjustment/1,
	 s8/3,
	 make_choice_reject_offer/1,
	 make_choice_resubmit_request/1,
	 make_choice_accept_offer/1,
	 s9/3,
	 s11/3,
	 s13/3,
	 s6/3]).

-include("agency.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), client_pid :: pid() | undefined, supplier_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_agency:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s3, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("agency initialized ~n", []),
    {ok, s3, Data}.

-spec s3(cast, {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s4, state_data(), [{next_event, internal, {price_quote}}]}.
s3(cast, {ClientPid, {booking_request, Destination}}, Data) ->
    Data1 = connection(Data),
    io:format("Agency: s3 Received booking_request from Client ~p for Destination ~p~n", [ClientPid, Destination]),
    {next_state, s4, Data1, [{next_event, internal, {price_quote}}]}.


-spec s4(internal, {atom()}, state_data()) -> 
    {next_state, s8, state_data(), [{next_event, internal, {price_adjustment}}]}.
s4(internal, {price_quote}, #state_data{client_pid = ClientPid} = Data) ->
    Price = 42, 
    io:format("Agency: s4 Sending price_quote to Client ~p~n", [Price]),
    gen_agency:send_s4_price_quote(ClientPid, Price, Data),
    {next_state, s8, Data, [{next_event, internal, {price_adjustment}}]}.

-spec s11(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s11(internal, {reject_confirmation}, #state_data{client_pid = ClientPid} = Data) ->
    io:format("Agency: s11 Sending reject_confirmation to Client ~n", []),
    gen_agency:send_s11_reject_confirmation(ClientPid, Data),
    {stop, normal, Data}.

-spec s6(cast, {pid(), {atom()}}, state_data()) -> {stop, normal, state_data()}.
s6(cast, {ClientPid, {cancel_agency}}, #state_data{client_pid = ClientPid} = Data) ->
    {stop, normal, Data}.

-spec make_choice_resubmit_request(state_data()) -> integer().
make_choice_resubmit_request(_Data) ->
    rand:uniform(2).

-spec s13(internal, {atom()}, state_data()) -> {next_state, s3, state_data()}.
s13(internal, {repeat_confirmation}, #state_data{client_pid = ClientPid} = Data) ->
    io:format("Agency: s13 Sending repeat_confirmation to Client ~n", []),
    gen_agency:send_s13_repeat_confirmation(ClientPid, Data),
    {next_state, s3, Data}.

-spec make_choice_reject_offer(state_data()) -> integer().
make_choice_reject_offer(_Data) ->
    rand:uniform(2).

-spec s8(internal | cast, {atom()} | {pid(), {term()}}, state_data()) -> 
    {next_state, s6, state_data()} |
    {next_state, s11, state_data(), [{next_event, internal, {reject_confirmation}}]}|
    {next_state, s13, state_data(), [{next_event, internal, {repeat_confirmation}}]} |
    {next_state, s9, state_data(), [{next_event, internal, {accept_confirmation}}]} |
    {keep_state, state_data()}.
s8(internal, {price_adjustment}, #state_data{client_pid = ClientPid} = Data) ->
    case make_choice_price_adjustment(Data) of
        1 ->
            io:format("Agency: s8 Waiting for Client ~n", []),
            {keep_state, Data};
        2 ->
            Price = 100, % Example price, replace with actual logic
            io:format("Agency: s8 Sending price_adjustment to Client ~p ~n", [Price]),
            gen_agency:send_s8_price_adjustment(ClientPid, Price, Data),
            {next_state, s6, Data}
    end;
s8(cast, {ClientPid, {reject_offer}}, #state_data{client_pid = ClientPid} = Data) ->
    io:format("Agency: s8 Received reject_offer from Client ~n", []),
    case make_choice_reject_offer(Data) of
        1 ->
            {next_state, s11, Data, [{next_event, internal, {reject_confirmation}}]};
        2 ->
            Price = 100, % Example price, replace with actual logic
            io:format("Agency: s8 Sending price_adjustment to Client ~p ~n", [Price]),
            gen_agency:send_s8_price_adjustment(ClientPid, Price, Data),
            {next_state, s6, Data}
    end;
s8(cast, {ClientPid, {resubmit_request}}, #state_data{client_pid = ClientPid} = Data) ->
    io:format("Agency: s8 Received resubmit_request from Client ~n", []),

    case make_choice_resubmit_request(Data) of
        1 ->
            {next_state, s13, Data, [{next_event, internal, {repeat_confirmation}}]};
        2 ->
            Price = 100, % Example price, replace with actual logic
            io:format("Agency: s8 Sending price_adjustment to Client ~p ~n", [Price]),
            gen_agency:send_s8_price_adjustment(ClientPid, Price, Data),
            {next_state, s6, Data}
    end;
s8(cast, {ClientPid, {accept_offer}}, #state_data{client_pid = ClientPid} = Data) ->
    io:format("Agency: s8 Received accept_offer from Client ~n", []),
    case make_choice_accept_offer(Data) of
        1 ->
            {next_state, s9, Data, [{next_event, internal, {accept_confirmation}}]};
        2 ->
            Price = 100, % Example price, replace with actual logic
            io:format("Agency: s8 Sending price_adjustment to Client ~p ~n", [Price]),
            gen_agency:send_s8_price_adjustment(ClientPid, Price, Data),
            {next_state, s6, Data}
    end.

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {accept_confirmation}, #state_data{client_pid = ClientPid} = Data) ->
    io:format("Agency: s9 Sending accept_confirmation to Client ~n", []),
    gen_agency:send_s9_accept_confirmation(ClientPid, Data),
    {stop, normal, Data}.

-spec make_choice_accept_offer(state_data()) -> integer().
make_choice_accept_offer(_Data) ->
    rand:uniform(2).

-spec make_choice_price_adjustment(state_data()) -> integer().
make_choice_price_adjustment(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("agency connected ~n", []),
    ClientPid = case whereis(client) of
        undefined ->
            io:format("client is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(client);
        Pid_client ->
            Pid_client
    end,
    SupplierPid = case whereis(supplier) of
        undefined ->
            io:format("supplier is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(supplier);
        Pid_supplier ->
            Pid_supplier
    end,
    Data#state_data{client_pid = ClientPid, supplier_pid = SupplierPid}.

