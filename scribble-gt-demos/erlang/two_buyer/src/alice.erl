-module(alice).
-behaviour(gen_alice).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s4/3,
	 s5/3,
	 s6/3,
	 s9/3
	]).

-include("alice.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), seller_pid :: pid() | undefined, bob_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_alice:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data(), [{next_event, internal, {request_title}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("alice initialized ~n", []),
    {ok, s4, Data, [{next_event, internal, {request_title}}]}.

-spec s4(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s5, state_data()} | 
    {stop, normal, state_data()}.
s4(internal, {request_title}, Data) ->
    Data1 = connection(Data),
    SellerPid = Data1#state_data.seller_pid,
    Title = "The Answer to the Ultimate Question of Life, the Universe, and Everything",
    io:format("Alice: s4 Sending request_title ~p to Seller ~n", [Title]),
    gen_alice:send_s4_request_title(SellerPid, Title, Data),
    {next_state, s5, Data1};
s4(cast, {SellerPid, {not_available}}, #state_data{seller_pid = SellerPid} = Data) ->
    Data1 = connection(Data),
    io:format("Alice: s4 Received not_available from Seller ~n", []),
    {stop, normal, Data1}.

-spec s5(cast, {pid(), {atom(), term()} | {atom()}}, state_data()) -> 
    {next_state, s6, state_data(), [{next_event, internal, {contribution}}]}|
    {stop, normal, state_data()}.
s5(cast, {SellerPid, {price_quote, Price}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Alice: s5 Received price_quote ~p from Seller ~n", [Price]),
    {next_state, s6, Data, [{next_event, internal, {contribution}}]};
s5(cast, {_SellerPid, {not_available}}, Data) ->
    io:format("Alice: s5 Received not_available from Seller ~n", []),
    {stop, normal, Data}.

-spec s6(internal, {atom()}, state_data()) -> {next_state, s9, state_data()}.
s6(internal, {contribution}, #state_data{bob_pid = BobPid} = Data) ->
    Amount = 42, % Example amount
    io:format("Alice: s6 Sending contribution ~p to Bob ~n", [Amount]),
    gen_alice:send_s6_contribution(BobPid, Amount, Data),
    {next_state, s9, Data}.

-spec s9(cast, {pid(), {atom()} | {atom(), term()}}, state_data()) -> 
    {stop, normal, state_data()}.
s9(cast, {BobPid, {purchase_notification}}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Alice: s9 Received purchase_notification from Bob ~n", []),
    {stop, normal, Data};
s9(cast, {BobPid, {cancel_notification}}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Alice: s9 Received cancel_notification from Bob ~n", []),
    {stop, normal, Data};
s9(cast, {SellerPid, {response_timeout}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Alice: s9 Received response_timeout from Seller ~n", []),
    {stop, normal, Data}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("alice connected ~n", []),
    SellerPid = case whereis(seller) of
        undefined ->
            io:format("seller is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(seller);
        Pid_seller ->
            Pid_seller
    end,
    BobPid = case whereis(bob) of
        undefined ->
            io:format("bob is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(bob);
        Pid_bob ->
            Pid_bob
    end,
    Data#state_data{seller_pid = SellerPid, bob_pid = BobPid}.

