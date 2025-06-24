-module(logs).
-behaviour(gen_logs).

-export([init/1, callback_mode/0, start_link/0, s1/3, make_choice_s9/1, s9/3, s10/3, s13/3, s5/3, s6/3]).

-include("logs.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), controller_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_logs:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("logs initialized ~n", []),
    {ok, s1, Data}.

-spec retry_whereis(atom()) -> pid().
retry_whereis(Name) ->
    case whereis(Name) of
        undefined ->
            io:format("API: ~p is not available yet. Will retry...~n", [Name]),
            timer:sleep(100),
            retry_whereis(Name);
        Pid ->
            Pid
    end.

-spec connect(state_data()) -> state_data().
connect(Data) ->
    ControllerPid = retry_whereis(controller),
    Data#state_data{controller_pid = ControllerPid}.

-spec s5(internal, {atom()}, state_data()) -> {next_state, s6, state_data()}.
s5(internal, {ack}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Logs: s5 Sending ack to Controller ~n", []),
    gen_logs:send_s5_ack(ControllerPid, Data),
    {next_state, s6, Data}.

-spec s6(cast, {pid(), {atom(), term()}}, state_data()) -> {next_state, s9, state_data()} | {next_state, s9, state_data(), [term()]}.
s6(cast, {ControllerPid, {restart, Int}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Logs: s6 Received restart ~p from Controller ~p~n", [Int, ControllerPid]),
    case make_choice_s9(Data) of
        1 ->
            {next_state, s9, Data, [{next_event, internal, {log_success}}]};
        2 ->
            {next_state, s9, Data, [{next_event, internal, {log_failure}}]}
    end.

-spec s10(cast, {pid(), {atom()}}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {ack}}]} | {next_state, s9, state_data()} | {next_state, s9, state_data(), [term()]}.
s10(cast, {ControllerPid, {timeout}}, #state_data{controller_pid = ControllerPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {ack}}]};
s10(cast, {ControllerPid, {success_ack}}, #state_data{controller_pid = ControllerPid} = Data) ->
    case make_choice_s9(Data) of
        1 ->
            {next_state, s9, Data, [{next_event, internal, {log_success}}]};
        2 ->
            {next_state, s9, Data, [{next_event, internal, {log_failure}}]}
    end.

-spec s13(cast, {pid(), {atom(), term()} | {atom()}}, state_data()) -> {next_state, s9, state_data()} | {next_state, s9, state_data(), [term()]} | {next_state, s5, state_data(), [{next_event, internal, {ack}}]} | {stop, normal, state_data()}.
s13(cast, {ControllerPid, {restart_logging, Int}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Logs: s13 Received restart_logging ~p from Controller ~p~n", [Int, ControllerPid]),
    case make_choice_s9(Data) of
        1 ->
            {next_state, s9, Data, [{next_event, internal, {log_success}}]};
        2 ->
            {next_state, s9, Data, [{next_event, internal, {log_failure}}]}
    end;
s13(cast, {ControllerPid, {timeout}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Logs: s13 Received timeout from Controller ~p~n", [ControllerPid]),
    {next_state, s5, Data, [{next_event, internal, {ack}}]};
s13(cast, {ControllerPid, {stop_logging, Int}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Logs: s13 Received stop_logging ~p from Controller ~p~n", [Int, ControllerPid]),
    {stop, normal, Data}.

-spec s9(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) -> {next_state, s10, state_data()} | {next_state, s13, state_data()} | {next_state, s5, state_data(), [{next_event, internal, {ack}}]}.
s9(internal, {log_success}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Logs: s9 Sending log_success to Controller ~n", []),
    Int = make_choice_s9(Data),
    gen_logs:send_s9_log_success(ControllerPid, Int, Data),
    {next_state, s10, Data};
s9(internal, {log_failure}, #state_data{controller_pid = ControllerPid} = Data) ->
    Int = make_choice_s9(Data),
    io:format("Logs: s9 Sending log_failure to Controller ~n", []),
    gen_logs:send_s9_log_failure(ControllerPid, Int, Data),
    {next_state, s13, Data};
s9(cast, {ControllerPid, {timeout}}, #state_data{controller_pid = ControllerPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {ack}}]}.

-spec make_choice_s9(state_data()) -> integer().
make_choice_s9(_Data) ->
    rand:uniform(2).

-spec s1(cast, {pid(), {atom(), term()}}, state_data()) -> 
    {next_state, s9, state_data()} | 
    {next_state, s9, state_data(), [term()]}.
s1(cast, {ControllerPid, {start_logging, Int}}, Data) ->
    io:format("Logs: s1 Received start_logging ~p from Controller ~p~n", [Int, ControllerPid]),
    Data1 = connect(Data),
    case make_choice_s9(Data) of
        1 ->
            {next_state, s9, Data1, [{next_event, internal, {log_success}}]};
        2 ->
            {next_state, s9, Data1, [{next_event, internal, {log_failure}}]}
    end.

