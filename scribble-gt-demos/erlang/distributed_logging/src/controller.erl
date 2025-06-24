-module(controller).
-behaviour(gen_controller).

-export([init/1, callback_mode/0, start_link/0, s1/3, make_choice_timeout/1, s9/3, make_choice_log_failure/1, make_choice_log_success/1, s10/3, make_choice_s13/1, s13/3, s5/3, s6/3]).

-include("controller.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), logs_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_controller:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {start_logging}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("controller initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {start_logging}}]}.

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
    LogsPid = retry_whereis(logs),
    Data#state_data{logs_pid = LogsPid}.

-spec make_choice_timeout(state_data()) -> integer().
make_choice_timeout(_Data) ->
    rand:uniform(2).

-spec make_choice_log_success(state_data()) -> integer().
make_choice_log_success(_Data) ->
    rand:uniform(2).

-spec s5(cast, {pid(), {atom()}}, state_data()) -> {next_state, s6, state_data(), [{next_event, internal, {restart}}]}.
s5(cast, {LogsPid, {ack}}, #state_data{logs_pid = LogsPid} = Data) ->
    {next_state, s6, Data, [{next_event, internal, {restart}}]}.

-spec s6(internal, {atom()}, state_data()) -> {next_state, s9, state_data(), [{next_event, internal, {timeout}}]}.
s6(internal, {restart}, #state_data{logs_pid = LogsPid} = Data) ->
    io:format("Controller: s6 Sending restart to Logs ~n", []),
    Int = Data#state_data.mc_counter_1,
    gen_controller:send_s6_restart(LogsPid, Int, Data),
    {next_state, s9, Data, [{next_event, internal, {timeout}}]}.

-spec s10(internal, {atom()}, state_data()) -> {next_state, s9, state_data(), [{next_event, internal, {timeout}}]}.
s10(internal, {success_ack}, #state_data{logs_pid = LogsPid} = Data) ->
    io:format("Controller: s10 Sending success_ack to Logs ~n", []),
    gen_controller:send_s10_success_ack(LogsPid, Data),
    {next_state, s9, Data, [{next_event, internal, {timeout}}]}.

-spec make_choice_log_failure(state_data()) -> integer().
make_choice_log_failure(_Data) ->
    rand:uniform(2).

-spec s13(internal, {atom()}, state_data()) -> {next_state, s9, state_data(), [{next_event, internal, {timeout}}]} | {stop, normal, state_data()}.
s13(internal, {restart_logging}, #state_data{logs_pid = LogsPid} = Data) ->
    io:format("Controller: s13 Sending restart_logging to Logs ~n", []),
    Int = Data#state_data.mc_counter_1,
    gen_controller:send_s13_restart_logging(LogsPid, Int, Data),
    {next_state, s9, Data, [{next_event, internal, {timeout}}]};
s13(internal, {stop_logging}, #state_data{logs_pid = LogsPid} = Data) ->
    io:format("Controller: s13 Sending stop_logging to Logs ~n", []),
    Int = Data#state_data.mc_counter_1,
    gen_controller:send_s13_stop_logging(LogsPid, Int, Data),
    {stop, normal, Data}.

-spec make_choice_s13(state_data()) -> integer().
make_choice_s13(_Data) ->
    rand:uniform(2).

-spec s9(internal | EventType :: term(), {atom()} | {pid(), {term(), integer()}}, state_data()) -> 
    {next_state, s5, state_data()} | {next_state, s13, state_data()} | 
    {next_state, s13, state_data(), [term()]} | 
    {next_state, s10, state_data(), [{next_event, internal, {success_ack}}]} |
    {keep_state, state_data()}.
s9(internal, {timeout}, #state_data{logs_pid = LogsPid} = Data) ->
    case make_choice_timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            io:format("Controller: s9 Sending timeout to Logs ~n", []),
            gen_controller:send_s9_timeout(LogsPid, Data),
            {next_state, s5, Data}
    end;
s9(cast, {LogsPid, {log_failure, Int}}, #state_data{logs_pid = LogsPid} = Data) ->
    io:format("Controller: s9 Received log_failure ~p from Logs ~n", [Int]),
    case make_choice_log_failure(Data) of
        1 ->
            case make_choice_s13(Data) of
                1 ->
                    {next_state, s13, Data, [{next_event, internal, {restart_logging}}]};
                2 ->
                    {next_state, s13, Data, [{next_event, internal, {stop_logging}}]}
            end;
        2 ->
            io:format("Controller: s9 Sending timeout to Logs ~n", []),
            gen_controller:send_s9_timeout(LogsPid, Data),
            {next_state, s5, Data}
    end;
s9(cast, {LogsPid, {log_success, Int}}, #state_data{logs_pid = LogsPid} = Data) ->
    io:format("Controller: s9 Received log_success ~p from Logs ~n", [Int]),
    case make_choice_log_success(Data) of
        1 ->
            {next_state, s10, Data, [{next_event, internal, {success_ack}}]};
        2 ->
            io:format("Controller: s9 Sending timeout to Logs ~n", []),
            gen_controller:send_s9_timeout(LogsPid, Data),
            {next_state, s5, Data}
    end.

-spec s1(internal, {atom()}, state_data()) -> {next_state, s9, state_data(), [{next_event, internal, {timeout}}]}.
s1(internal, {start_logging}, Data) ->
    Data1 = connect(Data),
    LogsPid = Data1#state_data.logs_pid,
    io:format("Controller: s1 Sending start_logging to Logs ~n", []),
    Int = Data1#state_data.mc_counter_1,
    gen_controller:send_s1_start_logging(LogsPid, Int, Data1),
    {next_state, s9, Data1, [{next_event, internal, {timeout}}]}.

