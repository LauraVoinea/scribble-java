%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for module b.
%%% Uses the state record defined in b.hrl.
%%%-------------------------------------------------------------------
-module(test_b).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
%%-include_lib("meck/include/meck.hrl").

-include("../src/b.hrl").

%%--------------------------------------------------------------------
%% Helpers for test outcome.
%%
succeed() -> ok.
fail() -> throw(failed).
%%fail_msg(Msg) -> throw({failed, Msg}).
fail_msg(Msg) -> throw({failed, lists:flatten(Msg)}).


%%--------------------------------------------------------------------
%% Macro to create an initial state for testing.
%%
-define(MAKE_STATE(), #state_data{mc_counter_1 = 0, a_pid = self(), c_pid = self()}).

safe_unregister(Name) ->
  case whereis(Name) of
    undefined -> ok;
    _ -> unregister(Name)
  end.


%%--------------------------------------------------------------------
%% Helper to set up registrations.
%%
%% This function registers the current process under names "a" and "c",
%% runs a provided function F, and then unregisters the names.
%%
setup_registrations(F) ->
%%  register(a, self()),
%%  register(c, self()),
  try F() of
    Result ->
      safe_unregister(a),
      safe_unregister(c),
      Result
  catch
    Class:Reason:Stack ->
      safe_unregister(a),
      safe_unregister(c),
      erlang:error({Class, Reason, Stack})
  end.

%%--------------------------------------------------------------------
%% Test for b:init/1
%%
init_test_() ->
  {setup,
    fun () -> setup_registrations(fun () -> ok end) end,
    fun (_) -> ok end,
    ?_test(
      begin
        {ok, _StateName, State, Events} = b:init([]),
        ?assertEqual(0, State#state_data.mc_counter_1),
        ?assertMatch([{next_event, internal, {'To'}}], Events)
      end
    )
  }.

%%--------------------------------------------------------------------
%% Test for the s5/3 clause handling a {a1} cast message.
%%
s5_a1_test_() ->
  ?_test(
    begin
      S0 = ?MAKE_STATE(),
      StateWithInc = S0#state_data{mc_counter_1 = 1},
      Result = b:s5(cast, {self(), {a1}}, StateWithInc),
      case Result of
        {next_state, s6, NewState, [{next_event, internal, {a2}}]} ->
          ?assertEqual(1, NewState#state_data.mc_counter_1);
        {next_state, s3, NewState, [{next_event, internal, {'To'}}]} ->
          ?assertEqual(1, NewState#state_data.mc_counter_1);
        Other ->
          fail_msg(io_lib:format("Unexpected result from s5 for {a1} message: ~p", [Other]))
      end
    end
  ).

%%s5_a1_test_() ->
%%  meck:new(b, [passthrough]),
%%  meck:expect(b, make_choice_a1, fun (_Data) -> 1 end),
%%%%  StateWithInc = ?MAKE_STATE()#state_data{mc_counter_1 = 1},
%%  S0 = ?MAKE_STATE(),
%%  Result = b:s5(cast, {self(), {a1}}, S0),
%%  %% Now verify that the result uses the forced value (1)
%%  case Result of
%%    {next_state, s6, NewState, [{next_event, internal, {a2}}]} ->
%%      ?assertEqual(1, NewState#state_data.mc_counter_1);
%%    {next_state, s3, NewState, [{next_event, internal, {'To'}}]} ->
%%      ?assertEqual(1, NewState#state_data.mc_counter_1);
%%    Other ->
%%      fail_msg(io_lib:flatten(io_lib:format("Unexpected result from s5 for {a1} message: ~p", [Other])))
%%  end,
%%  meck:unload(b).


%%--------------------------------------------------------------------
%% Test for the s5/3 clause handling an internal {'To'} message.
%%
s5_To_test_() ->
  ?_test(
    begin
      StateData = ?MAKE_STATE(),
      Result = b:s5(internal, {'To'}, StateData),
      case Result of
        {keep_state, NewState} ->
          ?assertEqual(1, NewState#state_data.mc_counter_1);
        {next_state, s3, NewState, [{next_event, internal, {'To'}}]} ->
          ?assertEqual(1, NewState#state_data.mc_counter_1);
        Other ->
          fail_msg(io_lib:format("Unexpected result from s5 for internal {'To'} message: ~p", [Other]))
      end
    end
  ).

%%--------------------------------------------------------------------
%% Test for s6/3 with an internal {a2} message.
%%
s6_test_() ->
  ?_test(
    begin
      Result = b:s6(internal, {a2}, ?MAKE_STATE()),
      ?assertMatch({next_state, s7, _NewState, [{next_event, internal, {a5}}]}, Result)
    end
  ).

%%--------------------------------------------------------------------
%% Test for s7/3 with an internal {a5} message.
%%
s7_test_() ->
  ?_test(
    begin
      Result = b:s7(internal, {a5}, ?MAKE_STATE()),
      ?assertMatch({stop, normal, _NewState}, Result)
    end
  ).

%%--------------------------------------------------------------------
%% Test for s3/3 with an internal {'To'} message.
%%
s3_test_() ->
  ?_test(
    begin
      Result = b:s3(internal, {'To'}, ?MAKE_STATE()),
      ?assertMatch({stop, normal, _NewState}, Result)
    end
  ).

%%--------------------------------------------------------------------
%% Aggregator: All tests in this module.
%%
all_test_() ->
  [init_test_(),
    s5_a1_test_(),
    s5_To_test_(),
    s6_test_(),
    s7_test_(),
    s3_test_()].
