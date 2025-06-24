-module(usr).
-behaviour(gen_user).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, s7/3]).

-include("usr.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, api_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_user:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("user initialized ~n", []),
    {ok, s1, Data}.

-spec connect(state_data()) -> state_data().
connect(Data) ->
    StoragePid = case whereis(storage) of
        undefined ->
            io:format("storage is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(storage);
        Pid ->
            Pid
    end,
    ApiPid = case whereis(api) of
        undefined ->
            io:format("api is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(api);
        Pid1 ->
            Pid1
    end,
    ControllerPid = case whereis(controller) of
        undefined ->
            io:format("controller is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(controller);
        Pid2 ->
            Pid2
    end,
    Data#state_data{storage_pid = StoragePid, api_pid = ApiPid, controller_pid = ControllerPid}.

-spec s1(cast, {pid(), {atom()}}, state_data()) -> {
    next_state, s3, state_data(), [{next_event, internal, {request}}]}.
s1(cast, {APIPid, {ready}}, Data) ->
    Data1 = connect(Data),
    io:format("User: s1 Received ready from API ~p~n", [APIPid]),
    {next_state, s3, Data1, [{next_event, internal, {request}}]}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s7, state_data()}.
s3(internal, {request}, #state_data{api_pid = APIPid} = Data) ->
    io:format("User: s3 Sending request to API ~n", []),
    gen_user:send_s3_request(APIPid, Data),
    {next_state, s7, Data}.

-spec s7(cast, {pid(), {atom()}}, state_data()) -> 
    {stop, normal, state_data()} |
    {next_state, s3, state_data(), [{next_event, internal, {request}}]}.
s7(cast, {APIPid, {api_response}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s3, Data, [{next_event, internal, {request}}]};
s7(cast, {APIPid, {shutdown_user}}, #state_data{api_pid = APIPid} = Data) ->
    {stop, normal, Data};
s7(cast, {APIPid, {error_response}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s3, Data, [{next_event, internal, {request}}]};
s7(cast, {APIPid, {timeout_notice}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s3, Data, [{next_event, internal, {request}}]}.

