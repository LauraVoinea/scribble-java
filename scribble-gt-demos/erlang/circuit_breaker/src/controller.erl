-module(controller).
-behaviour(gen_controller).

-export([init/1, callback_mode/0, start_link/0, s1/3, s3/3, s4/3, s6/3, make_choice_s11/1, s11/3, s12/3, s15/3, s16/3, s19/3, s20/3, s8/3]).

-include("controller.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), storage_pid :: pid() | undefined, api_pid :: pid() | undefined, user_pid :: pid() | undefined}.

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_controller:start_link(?MODULE, []).

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init(list()) -> {ok, s1, state_data(), [{next_event, internal, {start_storage}}]}.
init([]) ->
    Data = #state_data{mc_counter_1 = 0},
    io:format("controller initialized ~n", []),
    {ok, s1, Data, [{next_event, internal, {start_storage}}]}.

-spec connect(state_data()) -> state_data().
connect(Data) ->
    timer:sleep(1000),
    StoragePid = case whereis(storage) of
        undefined ->
            io:format("Controller: storage is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(storage);
        Pid ->
            Pid
    end,
    ApiPid = case whereis(api) of
        undefined ->
            io:format("Controller: api is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(api);
        Pid1 ->
            Pid1
    end,
    UserPid = case whereis(user) of
        undefined ->
            io:format("Controller: user is not available yet. Will retry...~n", []),
            timer:sleep(1000),
            whereis(user);
        Pid2 ->
            Pid2
    end,
    Data#state_data{user_pid = UserPid, storage_pid = StoragePid, api_pid = ApiPid}.

-spec s20(internal, {atom()}, state_data()) -> {stop, normal, state_data()}.
s20(internal, {shutdown_storage}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("Controller: s20 Sending shutdown_storage to Storage ~n", []),
    gen_controller:send_s20_shutdown_storage(StoragePid, Data),
    {stop, normal, Data}.

-spec s3(internal, {atom()}, state_data()) -> {next_state, s4, state_data()}.
s3(internal, {start_controller}, #state_data{api_pid = APIPid} = Data) ->
    io:format("Controller: s3 Sending start_controller to API ~n", []),
    gen_controller:send_s3_start_controller(APIPid, Data),
    {next_state, s4, Data}.

-spec s4(cast, {pid(), {atom()}}, state_data()) -> {next_state, s6, state_data()}.
s4(cast, {StoragePid, {hard_ping}}, #state_data{storage_pid = StoragePid} = Data) ->
    {next_state, s6, Data}.

-spec s11(internal | cast, {atom()} | {pid(), {atom(), term()}}, state_data()) ->
    {next_state, s12, state_data()} |
    {next_state, s15, state_data()} |
    {next_state, s19, state_data()} |
    {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]}.
s11(internal, {service_operational}, #state_data{api_pid = APIPid} = Data) ->
    io:format("Controller: s11 Sending service_operational to API ~n", []),
    gen_controller:send_s11_service_operational(APIPid, Data),
    {next_state, s12, Data};
s11(internal, {error_notice}, #state_data{api_pid = APIPid} = Data) ->
    io:format("Controller: s11 Sending error_notice to API ~n", []),
    gen_controller:send_s11_error_notice(APIPid, Data),
    {next_state, s15, Data};
s11(internal, {shutdown_api}, #state_data{api_pid = APIPid} = Data) ->
    io:format("Controller: s11 Sending shutdown_api to API ~n", []),
    gen_controller:send_s11_shutdown_api(APIPid, Data),
    {next_state, s19, Data};
s11(cast, {APIPid, {timeout}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}.

-spec s6(cast, {pid(), {atom()}}, state_data()) ->
    {next_state, s11, state_data(), [{next_event, internal, {error_notice}}]} |
    {next_state, s11, state_data(), [{next_event, internal, {service_operational}}]} |
    {next_state, s11, state_data(), [{next_event, internal, {shutdown_api}}]}.
s6(cast, {APIPid, {get_mode}}, #state_data{api_pid = APIPid} = Data) ->
    case make_choice_s11(Data) of
        1 ->
            {next_state, s11, Data, [{next_event, internal, {error_notice}}]};
        2 ->
            {next_state, s11, Data, [{next_event, internal, {service_operational}}]};
        3 ->
            {next_state, s11, Data, [{next_event, internal, {shutdown_api}}]}
    end.

-spec s8(internal, {atom()}, state_data()) -> {next_state, s6, state_data()}.
s8(internal, {timeout_notice}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("Controller: s8 Sending timeout_notice to Storage ~n", []),
    gen_controller:send_s8_timeout_notice(StoragePid, Data),
    {next_state, s6, Data}.

-spec s12(cast, {pid(), {atom()}}, state_data()) ->
    {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]} |
    {next_state, s6, state_data()}.
s12(cast, {APIPid, {timeout}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]};
s12(cast, {APIPid, {ack}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s6, Data}.

-spec s15(cast, {pid(), {atom()}}, state_data()) ->
    {next_state, s16, state_data(), [{next_event, internal, {storage_restart}}]} | {
        next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]}.
s15(cast, {APIPid, {error_ack}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s16, Data, [{next_event, internal, {storage_restart}}]};
s15(cast, {APIPid, {timeout}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}.

-spec make_choice_s11(state_data()) -> integer().
make_choice_s11(_Data) ->
    rand:uniform(3).

-spec s16(internal, {atom()}, state_data()) -> {next_state, s6, state_data()}.
s16(internal, {storage_restart}, #state_data{storage_pid = StoragePid} = Data) ->
    io:format("Controller: s16 Sending storage_restart to Storage ~n", []),
    gen_controller:send_s16_storage_restart(StoragePid, Data),
    {next_state, s6, Data}.

-spec s19(cast, {pid(), {atom()}}, state_data()) ->
    {next_state, s20, state_data(), [{next_event, internal, {shutdown_storage}}]} |
    {next_state, s8, state_data(), [{next_event, internal, {timeout_notice}}]}.
s19(cast, {APIPid, {shutdown_ack}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s20, Data, [{next_event, internal, {shutdown_storage}}]};
s19(cast, {APIPid, {timeout}}, #state_data{api_pid = APIPid} = Data) ->
    {next_state, s8, Data, [{next_event, internal, {timeout_notice}}]}.

-spec s1(internal, {atom()}, state_data()) ->
    {next_state, s3, state_data(), [{next_event, internal, {start_controller}}]}.
s1(internal, {start_storage},  Data) ->
    NewData = connect(Data),
    io:format("Controller: s1 Sending start_storage to Storage ~n", []),
    StoragePid = NewData#state_data.storage_pid,
    gen_controller:send_s1_start_storage(StoragePid, NewData),
    {next_state, s3, NewData, [{next_event, internal, {start_controller}}]}.

