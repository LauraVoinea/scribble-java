-module(api).
-behaviour(gen_api).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, s4/3, s5/3, make_choice_timeout/1, s10/3, make_choice_service_operational/1, make_choice_shutdown_api/1, make_choice_error_notice/1, s11/3, s12/3, s13/3, s14/3, s17/3, s18/3, s19/3, s22/3, s23/3, s24/3, s7/3]).

-include("api.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, user_pid :: pid() | undefined, controller_pid :: pid() | undefined}.

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
    Data#state_data{storage_pid = StoragePid, controller_pid = ControllerPid, user_pid = UserPid}.

-spec s1(cast, {pid(), {atom()}}, state_data()) -> 
    {next_state, s3, state_data(), [{next_event, internal, {ready}}]}.
s1(cast, {ControllerPid, {start_controller}}, Data) ->
    io:format("API: s1 Received start_controller from Controller ~p~n", [ControllerPid]),
    NewData = connect(Data),
    {next_state, s3, NewData, [{next_event, internal, {ready}}]}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s4, state_data()}.
s3(internal, {ready}, #state_data{user_pid = UserPid} = Data) ->
    io:format("API: s3 Sending ready to User ~n", []),
    gen_api:send_s3_ready(UserPid, Data),
    {next_state, s4, Data}.

-spec s4(cast, {pid(), {atom()}}, state_data()) -> {next_state, s5, state_data(), [{next_event, internal, {get_mode}}]}.
s4(cast, {UserPid, {request}}, #state_data{user_pid = UserPid} = Data) ->
    {next_state, s5, Data, [{next_event, internal, {get_mode}}]}.

-spec s5(internal, {atom()}, state_data()) -> {next_state, s10, state_data(), [{next_event, internal, {timeout}}]} | {keep_state, state_data()}.
s5(internal, {get_mode}, #state_data{controller_pid = ControllerPid} = Data) when is_pid(ControllerPid) ->
    io:format("API: s5 Sending get_mode to Controller ~n", []),
    gen_api:send_s5_get_mode(ControllerPid, Data),
    {next_state, s10, Data, [{next_event, internal, {timeout}}]};
s5(internal, {get_mode}, Data) ->
    {keep_state, Data}.

-spec s7(internal, {atom()}, state_data()) -> {next_state, s4, state_data()} | {keep_state, state_data()}.
s7(internal, {timeout_notice}, #state_data{user_pid = UserPid} = Data) when is_pid(UserPid) ->
    io:format("API: s7 Sending timeout_notice to User ~n", []),
    gen_api:send_s7_timeout_notice(UserPid, Data),
    {next_state, s4, Data}.

-spec make_choice_service_operational(state_data()) -> integer().
make_choice_service_operational(_Data) ->
    rand:uniform(2).

-spec make_choice_shutdown_api(state_data()) -> integer().
make_choice_shutdown_api(_Data) ->
    rand:uniform(2).

-spec make_choice_timeout(state_data()) -> integer().
make_choice_timeout(_Data) ->
    rand:uniform(2).

-spec s22(internal, {atom()}, state_data()) -> {next_state, s23, state_data(), [{next_event, internal, {prepare_shutdown}}]} | {keep_state, state_data()}.
s22(internal, {shutdown_ack}, #state_data{controller_pid = ControllerPid} = Data) when is_pid(ControllerPid) ->
    io:format("API: s22 Sending shutdown_ack to Controller ~n", []),
    gen_api:send_s22_shutdown_ack(ControllerPid, Data),
    {next_state, s23, Data, [{next_event, internal, {prepare_shutdown}}]};
s22(internal, {shutdown_ack}, Data) ->
    {keep_state, Data}.

-spec s11(internal, {atom()}, state_data()) -> {next_state, s12, state_data(), [{next_event, internal, {storage_request}}]} | {keep_state, state_data()}.
s11(internal, {ack}, #state_data{controller_pid = ControllerPid} = Data) when is_pid(ControllerPid) ->
    io:format("API: s11 Sending ack to Controller ~n", []),
    gen_api:send_s11_ack(ControllerPid, Data),
    {next_state, s12, Data, [{next_event, internal, {storage_request}}]};
s11(internal, {ack}, Data) ->
    {keep_state, Data}.

-spec s10(internal | EventType :: term(), {atom()} | {pid(), {atom()}}, state_data()) -> 
    {next_state, s7, state_data(), [{next_event, internal, {timeout_notice}}]} | 
    {next_state, s11, state_data(), [{next_event, internal, {ack}}]} | 
    {next_state, s22, state_data(), [{next_event, internal, {shutdown_ack}}]} | 
    {next_state, s17, state_data(), [{next_event, internal, {error_ack}}]} |
    {keep_state, state_data()}.
s10(internal, {timeout}, #state_data{controller_pid = ControllerPid} = Data) when is_pid(ControllerPid) ->
    case make_choice_timeout(Data) of
        1 ->
            {keep_state, Data};
        2 ->
            io:format("API: s10 Sending timeout to Controller ~n", []),
            gen_api:send_s10_timeout(ControllerPid, Data),
            {next_state, s7, Data, [{next_event, internal, {timeout_notice}}]}
    end;
s10(cast, {ControllerPid, {service_operational}}, #state_data{controller_pid = ControllerPid} = Data) ->
    case make_choice_service_operational(Data) of
        1 ->
            {next_state, s11, Data, [{next_event, internal, {ack}}]};
        2 ->
            io:format("API: s10 Sending timeout to Controller ~n", []),
            gen_api:send_s10_timeout(ControllerPid, Data),
            {next_state, s7, Data, [{next_event, internal, {timeout_notice}}]}
    end;
s10(cast, {ControllerPid, {shutdown_api}}, #state_data{controller_pid = ControllerPid} = Data) ->
    case make_choice_shutdown_api(Data) of
        1 ->
            {next_state, s22, Data, [{next_event, internal, {shutdown_ack}}]};
        2 ->
            io:format("API: s10 Sending timeout to Controller ~n", []),
            gen_api:send_s10_timeout(ControllerPid, Data),
            {next_state, s7, Data, [{next_event, internal, {timeout_notice}}]}
    end;
s10(cast, {ControllerPid, {error_notice}}, #state_data{controller_pid = ControllerPid} = Data) ->
    case make_choice_error_notice(Data) of
        1 ->
            {next_state, s17, Data, [{next_event, internal, {error_ack}}]};
        2 ->
            io:format("API: s10 Sending timeout to Controller ~n", []),
            gen_api:send_s10_timeout(ControllerPid, Data),
            {next_state, s7, Data, [{next_event, internal, {timeout_notice}}]}
    end.

-spec s24(internal, {atom()}, state_data()) -> {stop, normal, state_data()} | {keep_state, state_data()}.
s24(internal, {shutdown_user}, #state_data{user_pid = UserPid} = Data) when is_pid(UserPid) ->
    io:format("API: s24 Sending shutdown_user to User ~n", []),
    gen_api:send_s24_shutdown_user(UserPid, Data),
    {stop, normal, Data};
s24(internal, {shutdown_user}, Data) ->
    {keep_state, Data}.

-spec s13(cast, {pid(), {atom()}}, state_data()) -> {next_state, s14, state_data(), [{next_event, internal, {api_response}}]}.
s13(cast, {StoragePid, {storage_reponse}}, #state_data{storage_pid = StoragePid} = Data) ->
    {next_state, s14, Data, [{next_event, internal, {api_response}}]}.

-spec s23(internal, {atom()}, state_data()) -> {next_state, s24, state_data(), [{next_event, internal, {shutdown_user}}]} | {keep_state, state_data()}.
s23(internal, {prepare_shutdown}, #state_data{storage_pid = StoragePid} = Data) when is_pid(StoragePid) ->
    io:format("API: s23 Sending prepare_shutdown to Storage ~n", []),
    gen_api:send_s23_prepare_shutdown(StoragePid, Data),
    {next_state, s24, Data, [{next_event, internal, {shutdown_user}}]};
s23(internal, {prepare_shutdown}, Data) ->
    {keep_state, Data}.

-spec s12(internal, {atom()}, state_data()) -> {next_state, s13, state_data()} | {keep_state, state_data()}.
s12(internal, {storage_request}, #state_data{storage_pid = StoragePid} = Data) when is_pid(StoragePid) ->
    io:format("API: s12 Sending storage_request to Storage ~n", []),
    gen_api:send_s12_storage_request(StoragePid, Data),
    {next_state, s13, Data};
s12(internal, {storage_request}, Data) ->
    {keep_state, Data}.

-spec make_choice_error_notice(state_data()) -> integer().
make_choice_error_notice(_Data) ->
    rand:uniform(2).

-spec s14(internal, {atom()}, state_data()) -> {next_state, s4, state_data()} | {keep_state, state_data()}.
s14(internal, {api_response}, #state_data{user_pid = UserPid} = Data) when is_pid(UserPid) ->
    io:format("API: s14 Sending api_response to User ~n", []),
    gen_api:send_s14_api_response(UserPid, Data),
    {next_state, s4, Data};
s14(internal, {api_response}, Data) ->
    {keep_state, Data}.

-spec s17(internal, {atom()}, state_data()) -> {next_state, s18, state_data(), [{next_event, internal, {cancel_ack}}]} | {keep_state, state_data()}.
s17(internal, {error_ack}, #state_data{controller_pid = ControllerPid} = Data) when is_pid(ControllerPid) ->
    io:format("API: s17 Sending error_ack to Controller ~n", []),
    gen_api:send_s17_error_ack(ControllerPid, Data),
    {next_state, s18, Data, [{next_event, internal, {cancel_ack}}]};
s17(internal, {error_ack}, Data) ->
    {keep_state, Data}.

-spec s19(internal, {atom()}, state_data()) -> {next_state, s4, state_data()} | {keep_state, state_data()}.
s19(internal, {error_response}, #state_data{user_pid = UserPid} = Data) when is_pid(UserPid) ->
    io:format("API: s19 Sending error_response to User ~n", []),
    gen_api:send_s19_error_response(UserPid, Data),
    {next_state, s4, Data};
s19(internal, {error_response}, Data) ->
    {keep_state, Data}.

-spec s18(internal, {atom()}, state_data()) -> {next_state, s19, state_data(), [{next_event, internal, {error_response}}]} | {keep_state, state_data()}.
s18(internal, {cancel_ack}, #state_data{storage_pid = StoragePid} = Data) when is_pid(StoragePid) ->
    io:format("API: s18 Sending cancel_ack to Storage ~n", []),
    gen_api:send_s18_cancel_ack(StoragePid, Data),
    {next_state, s19, Data, [{next_event, internal, {error_response}}]};
s18(internal, {cancel_ack}, Data) ->
    {keep_state, Data}.



