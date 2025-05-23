%%%-------------------------------------------------------------------
%%% @author crypt
%%% @copyright (C) 2025, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Apr 2025 15:29
%%%-------------------------------------------------------------------
-module(test_timeout).
-author("crypt").
-include_lib("eunit/include/eunit.hrl").

timeout_application_test_() ->
  {setup,
    fun() ->
      application:start(timeout)
    end,
    fun(_) ->
      application:stop(timeout)
    end,
    fun() ->
      %% Verify the application is running by checking its entry in which_applications/0.
      Apps = application:which_applications(),
      ?assertMatch([{timeout, _, _} | _], Apps)
    end}.