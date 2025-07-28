-module(storage).
-behaviour(gen_storage).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, s8/3, s9/3, s12/3, s15/3]).

-include("storage.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), api_pid :: pid() | undefined, usr_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_storage:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("storage initialized ~n", []),
    {ok, s1, Data}.

-spec connect(state_data()) -> state_data().
connect(Data) ->
    ControllerPid = case whereis(controller) of
        undefined ->
            io:format("controller is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(controller);
        Pid ->
            Pid
    end,
    APIPid = case whereis(api) of
        undefined ->
            io:format("api is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(api);
        Pid1 ->
            Pid1
    end,
    UserPid = case whereis(user) of
        undefined ->
            io:format("user is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(user);
        Pid2 ->
            Pid2
    end,
    Data#state_data{controller_pid = ControllerPid, api_pid = APIPid, usr_pid = UserPid}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s8, state_data()}.
s3(internal, {hard_ping}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Storage: s3 Sending hard_ping to Controller ~n", []),
    gen_storage:send_s3_hard_ping(ControllerPid, Data),
    {next_state, s8, Data}.

-spec s12(cast, {pid(), {atom()}}, state_data()) -> {next_state, s8, state_data()}.
s12(cast, {ControllerPid, {storage_restart}}, #state_data{controller_pid = ControllerPid} = Data) ->
    {next_state, s8, Data}.

-spec s8(cast, {pid(), {atom()}}, state_data()) ->
    {next_state, s9, state_data(), [{next_event, internal, {'storage_reponse'}}]} |
    {next_state, s12, state_data()} |
    {next_state, s15, state_data()} |
    {next_state, s8, state_data()}.
s8(cast, {APIPid, {storage_request}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s9, Data, [{next_event, internal, {'storage_reponse'}}]};
s8(cast, {APIPid, {cancel_ack}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s12, Data};
s8(cast, {APIPid, {prepare_shutdown}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s15, Data};
s8(cast, {ControllerPid, {timeout_notice}}, #state_data{controller_pid = ControllerPid} = Data) ->
    {next_state, s8, Data}.

-spec s15(cast, {pid(), {atom()}}, state_data()) -> {stop, normal, state_data()}.
s15(cast, {ControllerPid, {shutdown_storage}}, #state_data{controller_pid = ControllerPid} = Data) ->
    {stop, normal, Data}.

-spec s9(internal, {atom()}, state_data()) ->
    {next_state, s8, state_data()}.
s9(internal, {storage_reponse}, #state_data{api_pid = APIPid} = Data) ->
    io:format("Storage: s9 Sending storage_reponse to API ~n", []),
    gen_storage:send_s9_storage_reponse(APIPid, Data),
    {next_state, s8, Data}.

-spec s1(cast | info, {pid(), {atom()}} | {atom(), pid()}, state_data()) -> 
    {next_state, s3, state_data(), [{next_event, internal, {hard_ping}}]}.
s1(cast, {ControllerPid, {start_storage}}, Data) ->
    io:format("Storage: s1 Received start_storage from Controller ~p~n", [ControllerPid]),
    NewData = connect(Data),
    {next_state, s3, NewData, [{next_event, internal, {hard_ping}}]}.

