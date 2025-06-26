-module(supplier).
-behaviour(gen_supplier).

-export([init/1, callback_mode/0, start_link/0, s5/3, s6/3]).

-include("supplier.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), agency_pid :: pid() | undefined, client_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_supplier:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s5, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("supplier initialized ~n", []),
    {ok, s5, Data}.

-spec s5(cast, {pid(), {atom()} | {atom(), term()} | {atom(), term(), term()}}, state_data()) -> 
    {stop, normal, state_data()} | 
    {next_state, s5, state_data()} | 
    {next_state, s6, state_data(), [{next_event, internal, {confirm_date}}]} |
    {stop, normal, state_data()}.
s5(cast, {ClientPid, {cancel_booking}}, Data) ->
    Data1 = connection(Data),
    io:format("Supplier: s5 Received cancel_booking from Client ~p~n", [ClientPid]),
    {stop, normal, Data1};
s5(cast, {ClientPid, {provide_address, Address}}, Data) ->
    Data1 = connection(Data),
    io:format("Supplier: s5 Received provide_address from Client ~p with Address ~p~n", [ClientPid, Address]),
    {next_state, s6, Data1, [{next_event, internal, {confirm_date}}]};
s5(cast, {ClientPid, {resubmitting}}, Data) ->
    Data1 = connection(Data),
    io:format("Supplier: s5 Received resubmitting from Client ~p~n", [ClientPid]),
    {next_state, s5, Data1};
s5(cast, {ClientPid, {cancel_supplier}}, Data) ->
    Data1 = connection(Data),
    io:format("Supplier: s5 Received cancel_supplier from Client ~p~n", [ClientPid]),
    {stop, normal, Data1}.

-spec s6(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(internal, {confirm_date}, #state_data{client_pid = ClientPid} = Data) ->
    Date = calendar:local_time_to_universal_time_dst(calendar:local_time()),
    io:format("Supplier: s6 Sending confirm_date to Client ~p~n", [Date]),
    gen_supplier:send_s6_confirm_date(ClientPid, Date, Data),
    {stop, normal, Data}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("supplier connected ~n", []),
    AgencyPid = case whereis(agency) of
        undefined ->
            io:format("agency is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(agency);
        Pid_agency ->
            Pid_agency
    end,
    ClientPid = case whereis(client) of
        undefined ->
            io:format("client is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(client);
        Pid_client ->
            Pid_client
    end,
    Data#state_data{agency_pid = AgencyPid, client_pid = ClientPid}.

