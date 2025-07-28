-module(api).
-behaviour(gen_api).

-export([init/1,
    callback_mode/0,
    start_link/0,
    s1/3,
    s3/3,
    s5/3,
    s6/3,
    make_choice_timeout/1,
    s11/3,
    make_choice_error_notice/1,
    make_choice_shutdown_api/1,
    make_choice_service_operational/1,
    s12/3,
    s13/3,
    s14/3,
    s15/3,
    s18/3,
    s19/3,
    s20/3,
    s23/3,
    s24/3,
    s25/3,
    s8/3
]).

-include("api.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, usr_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_api:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data()}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("api initialized ~n", []),
    {ok, s1, Data}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s5, state_data()}.
s3(internal, {ready}, #state_data{usr_pid = UserPid} = Data) ->
    io:format("API: s3 Sending ready to User ~n", []),
    gen_api:send_s3_ready(UserPid, Data),
    {next_state, s5, Data}.

-spec s5(cast, {atom(), {request}}, state_data()) ->
    {next_state, s6, state_data(), [{next_event, internal, {get_mode}}]}.
s5(cast, {UserPid, {request}}, #state_data{usr_pid = UserPid} = Data) ->
    io:format("User: s5 Received request  from User ~p ~n", [UserPid]),
    {next_state, s6, Data, [{next_event, internal, {get_mode}}]}.

-spec s6(internal, {atom()}, state_data()) ->
    {next_state, s11, state_data(), [{next_event, internal, {timeout}}]}.
s6(internal, {get_mode}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("API: s6 Sending get_mode to Controller ~n", []),
    gen_api:send_s6_get_mode(ControllerPid, Data),
    {next_state, s11, Data, [{next_event, internal, {timeout}}]}.

-spec make_choice_service_operational(state_data()) -> integer().
make_choice_service_operational(_Data) ->
    rand:uniform(2).

-spec s8(internal, {atom()}, state_data()) -> {next_state, s5, state_data()}.
s8(internal, {timeout_notice}, #state_data{usr_pid = UserPid} = Data) ->
    io:format("API: s8 Sending timeout_notice to User ~n", []),
    gen_api:send_s8_timeout_notice(UserPid, Data),
    {next_state, s5, Data}.

-spec make_choice_shutdown_api(state_data()) -> integer().
make_choice_shutdown_api(_Data) ->
    rand:uniform(2).

-spec s20(internal, {atom()}, state_data()) -> {next_state, s5, state_data()}.
s20(internal, {error_response}, #state_data{usr_pid = UserPid} = Data) ->
    io:format("API: s20 Sending error_response to User ~n", []),
    gen_api:send_s20_error_response(UserPid, Data),
    {next_state, s5, Data}.

-spec make_choice_timeout(state_data()) -> integer().
make_choice_timeout(_Data) ->
    rand:uniform(2).

-spec s11(internal, {timeout}, state_data()) ->
    {keep_state, state_data()} |
    {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} |
    {next_state, s12, state_data(), [{next_event, internal, {ack}}]} |
    {next_state, s18, state_data(),[{next_event, internal, {error_ack}}]} |
    {next_state, s23, state_data(), [{next_event, internal, {shutdown_ack}}]}.
s11(internal, {timeout}, #state_data{controller_pid = ControllerPid} = Data) ->
    case make_choice_timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            gen_api:send_s11_timeout(ControllerPid, Data),
            io:format("API: s11 Sending timeout to Controller ~n", []),
            {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}
    end;
s11(cast, {ControllerPid, {error_notice}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Controller: s11 Received error_notice  from Controller ~p ~n", [ControllerPid]),
    case make_choice_error_notice(Data) of
        1 ->
            {next_state, s18, Data, [{next_event, internal, {error_ack}}]};
        2 ->
            gen_api:send_s11_timeout(ControllerPid, Data),
            {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}
    end;
s11(cast, {ControllerPid, {shutdown_api}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Controller: s11 Received shutdown_api  from Controller ~p ~n", [ControllerPid]),
    case make_choice_shutdown_api(Data) of
        1 ->
            {next_state, s23, Data, [{next_event, internal, {shutdown_ack}}]};
        2 ->
            gen_api:send_s11_timeout(ControllerPid, Data),
            {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}
    end;
s11(cast, {ControllerPid, {service_operational}}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("Controller: s11 Received service_operational  from Controller ~p ~n", [ControllerPid]),
    case make_choice_service_operational(Data) of
        1 ->
            {next_state, s12, Data, [{next_event, internal, {ack}}]};
        2 ->
            gen_api:send_s11_timeout(ControllerPid, Data),
            {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}
    end.

-spec s24(internal, {prepare_shutdown}, state_data()) ->
    {next_state, s25, state_data(), [{next_event, internal, {shutdown_user}}]}.
s24(internal, {prepare_shutdown}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("API: s24 Sending prepare_shutdown to Storage ~n", []),
    gen_api:send_s24_prepare_shutdown(StoragePid, Data),
    {next_state, s25, Data, [{next_event, internal, {shutdown_user}}]}.

-spec s13(internal, {atom()}, state_data()) -> {next_state, s14, state_data()}.
s13(internal, {storage_request}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("API: s13 Sending storage_request to Storage ~n", []),
    gen_api:send_s13_storage_request(StoragePid, Data),
    {next_state, s14, Data}.

-spec s23(internal, {shutdown_ack}, state_data()) ->
    {next_state, s24, state_data(), [{next_event, internal, {prepare_shutdown}}]}.
s23(internal, {shutdown_ack}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("API: s23 Sending shutdown_ack to Controller ~n", []),
    gen_api:send_s23_shutdown_ack(ControllerPid, Data),
    {next_state, s24, Data, [{next_event, internal, {prepare_shutdown}}]}.

-spec s12(internal, {ack}, state_data()) ->
    {next_state, s13, state_data(), [{next_event, internal, {storage_request}}]}.
s12(internal, {ack}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("API: s12 Sending ack to Controller ~n", []),
    gen_api:send_s12_ack(ControllerPid, Data),
    {next_state, s13, Data, [{next_event, internal, {storage_request}}]}.

-spec s15(internal, {atom()}, state_data()) -> {next_state, s5, state_data()}.
s15(internal, {api_response}, #state_data{usr_pid = UserPid} = Data) ->
    io:format("API: s15 Sending api_response to User ~n", []),
    gen_api:send_s15_api_response(UserPid, Data),
    {next_state, s5, Data}.

-spec make_choice_error_notice(state_data()) -> integer().
make_choice_error_notice(_Data) ->
    rand:uniform(2).

-spec s25(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s25(internal, {shutdown_user}, #state_data{usr_pid = UserPid} = Data) ->
    io:format("API: s25 Sending shutdown_user to User ~n", []),
    gen_api:send_s25_shutdown_user(UserPid, Data),
    {stop, normal, Data}.

-spec s14(cast, {storage_reponse}, state_data()) ->
    {next_state, s15, state_data(), [{next_event, internal, {api_response}}]}.
s14(cast, {StoragePid, {storage_reponse}}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("Storage: s14 Received storage_reponse  from Storage ~p ~n", [StoragePid]),
    {next_state, s15, Data, [{next_event, internal, {api_response}}]}.

-spec s19(internal, {cancel_ack}, state_data()) ->
    {next_state, s20, state_data(), [{next_event, internal, {error_response}}]}.
s19(internal, {cancel_ack}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("API: s19 Sending cancel_ack to Storage ~n", []),
    gen_api:send_s19_cancel_ack(StoragePid, Data),
    {next_state, s20, Data, [{next_event, internal, {error_response}}]}.

-spec s18(internal, {error_ack}, state_data()) ->
    {next_state, s19, state_data(), [{next_event, internal, {cancel_ack}}]}.
s18(internal, {error_ack}, #state_data{controller_pid = ControllerPid} = Data) ->
    io:format("API: s18 Sending error_ack to Controller ~n", []),
    gen_api:send_s18_error_ack(ControllerPid, Data),
    {next_state, s19, Data, [{next_event, internal, {cancel_ack}}]}.

-spec s1(cast, {pid(), {start_controller}}, state_data()) ->
    {next_state, s3, state_data(), [{next_event, internal, {ready}}]}.
s1(cast, {ControllerPid, {start_controller}}, Data) ->
    NewData = connect(Data),
    io:format("Controller: s1 Received start_controller  from Controller ~p ~n", [ControllerPid]),
    {next_state, s3, NewData, [{next_event, internal, {ready}}]}.

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
    StoragePid = retry_whereis(storage),
    ControllerPid = retry_whereis(controller),
    UserPid = retry_whereis(usr),
    Data#state_data{storage_pid = StoragePid, controller_pid = ControllerPid, usr_pid = UserPid}.

