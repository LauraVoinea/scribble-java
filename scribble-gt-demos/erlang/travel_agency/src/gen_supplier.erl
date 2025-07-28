-module(gen_supplier).
-behaviour(gen_statem).

-export([init/1, 
	 callback_mode/0, 
	 code_change/4, 
	 terminate/3, 
	 start_link/2, 
	 s5/3, 
	 send_s6_confirm_date/3, 
	 s6/3
	 ]).

-include("supplier.hrl").
-type state_data() :: #state_data{mc_counter_1 :: integer(), agency_pid :: pid() | undefined, client_pid :: pid() | undefined}.

-callback s5(EventType :: term(), {pid(), {term()}, integer()}, state_data()) -> {stop, normal, state_data()} | {next_state, s6, state_data(), [{next_event, internal, {confirm_date}}]} | {keep_state, state_data()} | {ok, s5, state_data()} | {keep_state, state_data(), [postpone]}.
-callback s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
-callback init(Args :: list()) -> 
	{ok, s5, state_data()}.

-spec start_link(CallbackModule :: module(), Args :: list()) ->
    {ok, pid()} | {error, term()}.
start_link(CallbackModule, Args) ->
    case code:ensure_loaded(CallbackModule) of
        {module, CallbackModule} ->
            gen_statem:start_link({local, CallbackModule}, gen_supplier, {CallbackModule, Args}, [{debug, [trace, {log_to_file, "supplier_debug.log"}]}]);
        {error, Reason} ->
            {error, Reason}
    end.

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({CallbackModule :: module(), Args :: list()}) -> 
	{ok, s5, state_data()}.
init({CallbackModule, _Args}) ->
    io:format("supplier: Initializing with callback module ~p~n", [CallbackModule]),
    put(callback_module, CallbackModule),
    CallbackModule:init([]).

-spec s5(EventType :: term(), {pid(), {term()}, integer()}, state_data()) ->
    {stop, normal, state_data()} |
    {next_state, s6, state_data(), [{next_event, internal, {confirm_date}}]} |
    {keep_state, state_data()} |
    {ok, s5, state_data()} |
    {keep_state, state_data(), [postpone]}.
s5(_EventType, {_Pid, {provide_address, Address}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_supplier: Postponing event ~p~n", [[provide_address, Address]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {cancel_supplier}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_supplier: Postponing event ~p~n", [[cancel_supplier]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {cancel_booking}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_supplier: Postponing event ~p~n", [[cancel_booking]]),
    {keep_state, Data, [postpone]};
s5(_EventType, {_Pid, {resubmitting}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter > MC + 1 ->
    io:format("gen_supplier: Postponing event ~p~n", [[resubmitting]]),
    {keep_state, Data, [postpone]};
s5(EventType, {ClientPid, {cancel_booking}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {ClientPid, {cancel_booking}}, NewData);
s5(EventType, {ClientPid, {provide_address, Address}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {ClientPid, {provide_address, Address}}, NewData);
s5(EventType, {ClientPid, {resubmitting}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {ClientPid, {resubmitting}}, NewData);
s5(EventType, {ClientPid, {cancel_supplier}, Counter}, #state_data{mc_counter_1 = MC} = Data) when Counter =:= MC + 1 ->
    NewData = Data#state_data{mc_counter_1 = MC + 1},
    CallbackModule = get(callback_module),
    CallbackModule:s5(EventType, {ClientPid, {cancel_supplier}}, NewData);
s5(_EventType, {_Pid, Msg, _Counter}, Data) when Msg =:= {resubmitting} 
		orelse Msg =:= {cancel_booking} 
		orelse Msg =:= {cancel_supplier} ->
    io:format("gen_supplier: Garbage collecting event ~p~n", [Msg]),
    {keep_state, Data};
s5(_EventType, {_Pid, {provide_address, Address}, _Counter}, Data) ->
    io:format("gen_supplier: Garbage collecting event ~p~n", [{provide_address, Address}]),
    {keep_state, Data}.

-spec s6(EventType :: term(), {atom()}, state_data()) -> {stop, normal, state_data()}.
s6(EventType, {confirm_date}, Data) ->
    CallbackModule = get(callback_module),
    CallbackModule:s6(EventType, {confirm_date}, Data).

-spec send_s6_confirm_date(ClientPid :: pid(), Date :: term(), Data :: state_data()) -> ok.
send_s6_confirm_date(ClientPid, Date, Data) ->
    Counter = Data#state_data.mc_counter_1,
    gen_statem:cast(ClientPid, {self(), {confirm_date, Date}, Counter}).

-spec code_change(OldVsn :: term(), StateName :: atom(), StateData :: state_data(), Extra :: term()) ->
    {ok, state_data()}.
code_change(_Vsn, _StateName, StateData, _Extra) ->
    {ok, StateData}.

-spec terminate(Reason :: term(), State :: atom(), Data :: state_data()) -> ok.
terminate(_Reason, _State, _StateData) ->
    ok.

