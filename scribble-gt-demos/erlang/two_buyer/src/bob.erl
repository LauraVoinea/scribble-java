-module(bob).
-behaviour(gen_bob).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 s4/3,
	 s5/3,
	 make_choice_s8/1,
	 s8/3,
	 s9/3,
	 s10/3,
	 s12/3,
	 s13/3
	]).

-include("bob.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), seller_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_bob:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s4, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("bob initialized ~n", []),
    {ok, s4, Data}.

-spec s4(cast, {pid(), {atom(), term()} | {atom()}}, state_data()) -> 
    {next_state, s5, state_data()} | 
    {stop, normal, state_data()}.
s4(cast, {SellerPid, {price_quote, Price}}, Data) ->
    Data1 = connection(Data),
    io:format("Bob: s4 Received price_quote ~p from Seller ~p ~n", [Price, SellerPid]),
    {next_state, s5, Data1};
s4(cast, {SellerPid, {not_available}}, Data) ->
    Data1 = connection(Data),
    io:format("Bob: s4 Received not_available from Seller ~p ~n", [SellerPid]),
    {stop, normal, Data1}.


-spec s5(cast, {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s8, state_data(), [{next_event, internal, {reject_quote}}] | [{next_event, internal, {accept_quote}}]} | 
    {stop, normal, state_data()}.
s5(cast, {AlicePid, {contribution, Amount}}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("Bob: s5 Received contribution ~p from Alice ~n", [Amount]),
    case make_choice_s8(Data) of
        1 ->
            io:format("Bob: s5 Choosing to reject quote ~n", []),
            {next_state, s8, Data, [{next_event, internal, {reject_quote}}]};
        2 ->
            io:format("Bob: s5 Choosing to accept quote ~n", []),
            {next_state, s8, Data, [{next_event, internal, {accept_quote}}]}
    end.

-spec s10(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s10(internal, {purchase_notification}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("Bob: s10 Sending purchase_notification to Alice ~n", []),
    gen_bob:send_s10_purchase_notification(AlicePid, Data),
    {stop, normal, Data}.

-spec s13(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s13(internal, {cancel_notification}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("Bob: s13 Sending cancel_notification to Alice ~n", []),
    gen_bob:send_s13_cancel_notification(AlicePid, Data),
    {stop, normal, Data}.

-spec s12(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s13, state_data(), [{next_event, internal, {cancel_notification}}]} |
    {stop, normal, state_data()}.
s12(cast, {SellerPid, {cancel_confirmation}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s12 Received cancel_confirmation from Seller ~n", []),
    {next_state, s13, Data, [{next_event, internal, {cancel_notification}}]};
s12(cast, {SellerPid, {response_timeout}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s12 Received response_timeout from Seller ~n", []),
    {stop, normal, Data}.

-spec s8(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s12, state_data()} | 
    {next_state, s9, state_data()} | 
    {stop, normal, state_data()}.
s8(internal, {reject_quote}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s8 Sending reject_quote to Seller ~n", []),
    gen_bob:send_s8_reject_quote(SellerPid, Data),
    {next_state, s12, Data};
s8(internal, {accept_quote}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s8 Sending accept_quote to Seller ~n", []),
    gen_bob:send_s8_accept_quote(SellerPid, Data),
    {next_state, s9, Data};
s8(cast, {SellerPid, {response_timeout}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s8 Received response_timeout from Seller ~n", []),
    {stop, normal, Data}.

-spec s9(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s10, state_data(), [{next_event, internal, {purchase_notification}}]} |
    {stop, normal, state_data()}.
s9(cast, {SellerPid, {response_timeout}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s9 Received response_timeout from Seller ~n", []),
    {stop, normal, Data};
s9(cast, {SellerPid, {purchase_confirmed}}, #state_data{seller_pid = SellerPid} = Data) ->
    io:format("Bob: s9 Received purchase_confirmed from Seller ~n", []),
    {next_state, s10, Data, [{next_event, internal, {purchase_notification}}]}.

-spec make_choice_s8(state_data()) -> integer().
make_choice_s8(_Data) ->
    rand:uniform(2).

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("bob connected ~n", []),
    SellerPid = case whereis(seller) of
        undefined ->
            io:format("seller is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(seller);
        Pid_seller ->
            Pid_seller
    end,
    AlicePid = case whereis(alice) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(alice);
        Pid_alice ->
            Pid_alice
    end,
    Data#state_data{seller_pid = SellerPid, alice_pid = AlicePid}.

