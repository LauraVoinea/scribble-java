-module(seller).
-behaviour(gen_seller).

-export([init/1,
	 callback_mode/0,
	 start_link/0,
	 make_choice_not_available/1,
	 s5/3,
	 make_choice_request_title/1,
	 s6/3,
	 s7/3,
	 make_choice_response_timeout/1,
	 s11/3,
	 make_choice_accept_quote/1,
	 make_choice_reject_quote/1,
	 s12/3,
	 s14/3,
	 s9/3,
	 s3/3
	]).

-include("seller.hrl").
-type state_data() :: #state_data{mc_counter_2 :: integer(), mc_counter_1 :: integer(), bob_pid :: pid() | undefined, alice_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_seller:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s5, state_data(), [{next_event, internal, {not_available}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("seller initialized ~n", []),
    {ok, s5, Data, [{next_event, internal, {not_available}}]}.

s5(internal, {not_available}, Data) ->
    Data1 = connection(Data),
    AlicePid = Data1#state_data.alice_pid,
    case make_choice_not_available(Data) of
        1 ->
            {keep_state, Data1};
        2 ->
            gen_seller:send_s5_not_available(AlicePid, Data),
            io:format("Seller: s5 Sending not_available to Alice ~n", []),
            {next_state, s3, Data1, [{next_event, internal, {not_available}}]}
    end;
s5(cast, {AlicePid, {request_title, Title}}, #state_data{alice_pid = AlicePid} = Data) ->
    Data1 = connection(Data),
    io:format("Seller: s5 Received request_title ~p from Alice ~n", [Title]),
    case make_choice_request_title(Data) of
        1 ->
            io:format("Seller: s5 Choosing price_quote ~n", []),
            {next_state, s6, Data1, [{next_event, internal, {price_quote}}]};
        2 ->
            io:format("Seller: s5 Choosing not_available ~n", []),
            gen_seller:send_s5_not_available(AlicePid, Data),
            {next_state, s3, Data1, [{next_event, internal, {not_available}}]}
    end.

-spec s3(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s3(internal, {not_available}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Seller: s3 Sending not_available to Bob ~n", []),
    gen_seller:send_s3_not_available(BobPid, Data),
    {stop, normal, Data}.

-spec make_choice_not_available(state_data()) -> integer().
make_choice_not_available(_Data) ->
    rand:uniform(2).
-spec s11(internal, {atom()}, state_data()) -> 
    {next_state, s9, state_data(), [{next_event, internal, {response_timeout}}]} |
    {keep_state, state_data()} |
    {next_state, s12, state_data(), [{next_event, internal, {purchase_confirmed}}]} |
    {next_state, s14, state_data(), [{next_event, internal, {cancel_confirmation}}]}.
s11(internal, {response_timeout}, #state_data{bob_pid = BobPid} = Data) ->
    case make_choice_response_timeout(Data) of
        1 ->
            io:format("Seller: s11 Waiting for Bob ~n", []),
            {keep_state, Data};
        2 ->
            gen_seller:send_s11_response_timeout(BobPid, Data),
            io:format("Seller: s11 Sending response_timeout to Bob ~n", []),
            {next_state, s9, Data, [{next_event, internal, {response_timeout}}]}
    end;
s11(cast, {BobPid, {accept_quote}}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Seller: s11 Received accept_quote from Bob ~n", []),
    case make_choice_accept_quote(Data) of
        1 ->
            io:format("Seller: s11 Choosing purchase_confirmed ~n", []),
            {next_state, s12, Data, [{next_event, internal, {purchase_confirmed}}]};
        2 ->
            io:format("Seller: s11 Choosing response_timeout ~n", []),
            gen_seller:send_s11_response_timeout(BobPid, Data),
            {next_state, s9, Data, [{next_event, internal, {response_timeout}}]}
    end;
s11(cast, {BobPid, {reject_quote}}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Seller: s11 Received reject_quote from Bob ~n", []),
    case make_choice_reject_quote(Data) of
        1 ->
            io:format("Seller: s11 Choosing cancel_confirmation ~n", []),
            {next_state, s14, Data, [{next_event, internal, {cancel_confirmation}}]};
        2 ->
            io:format("Seller: s11 Choosing response_timeout ~n", []),
            gen_seller:send_s11_response_timeout(BobPid, Data),
            {next_state, s9, Data, [{next_event, internal, {response_timeout}}]}
    end.

-spec make_choice_response_timeout(state_data()) -> integer().
make_choice_response_timeout(_Data) ->
    rand:uniform(2).

-spec s6(internal, {atom()}, state_data()) -> 
    {next_state, s7, state_data(), [{next_event, internal, {price_quote}}]} |
    {stop, normal, state_data()}.
s6(internal, {price_quote}, #state_data{alice_pid = AlicePid} = Data) ->
    Price = 100, % Example price quote
    io:format("Seller: s6 Sending price_quote ~p to Alice ~n", [Price]),
    gen_seller:send_s6_price_quote(AlicePid, Price, Data),
    {next_state, s7, Data, [{next_event, internal, {price_quote}}]}.

-spec make_choice_request_title(state_data()) -> integer().
make_choice_request_title(_Data) ->
    rand:uniform(2).

-spec s7(internal, {atom()}, state_data()) -> 
    {next_state, s11, state_data(), [{next_event, internal, {response_timeout}}]} |
    {stop, normal, state_data()}.   
s7(internal, {price_quote}, #state_data{bob_pid = BobPid} = Data) ->
    Price = 100, % Example price quote
    io:format("Seller: s7 Sending price_quote ~p to Bob ~n", [Price]),
    gen_seller:send_s7_price_quote(BobPid, Price, Data),
    {next_state, s11, Data, [{next_event, internal, {response_timeout}}]}.

-spec s12(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s12(internal, {purchase_confirmed}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Seller: s12 Sending purchase_confirmed to Bob ~n", []),
    gen_seller:send_s12_purchase_confirmed(BobPid, Data),
    {stop, normal, Data}.

-spec make_choice_accept_quote(state_data()) -> integer().
make_choice_accept_quote(_Data) ->
    rand:uniform(2).

-spec s9(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s9(internal, {response_timeout}, #state_data{alice_pid = AlicePid} = Data) ->
    io:format("Seller: s9 Sending response_timeout to Alice ~n", []),
    gen_seller:send_s9_response_timeout(AlicePid, Data),
    {stop, normal, Data}.

-spec make_choice_reject_quote(state_data()) -> integer().
make_choice_reject_quote(_Data) ->
    rand:uniform(2).

-spec s14(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s14(internal, {cancel_confirmation}, #state_data{bob_pid = BobPid} = Data) ->
    io:format("Seller: s14 Sending cancel_confirmation to Bob ~n", []),
    gen_seller:send_s14_cancel_confirmation(BobPid, Data),
    {stop, normal, Data}.

-spec connection(state_data()) -> state_data().
connection(Data) ->
    io:format("seller connected ~n", []),
    BobPid = case whereis(bob) of
        undefined ->
            io:format("bob is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(bob);
        Pid_bob ->
            Pid_bob
    end,
    AlicePid = case whereis(alice) of
        undefined ->
            io:format("alice is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(alice);
        Pid_alice ->
            Pid_alice
    end,
    Data#state_data{bob_pid = BobPid, alice_pid = AlicePid}.

