%%%-------------------------------------------------------------------
%% @doc amqp_fsm_client public API
%% @end
%%%-------------------------------------------------------------------

-module(amqp_fsm_client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  % Start the consumer
  case consumer:start_link() of
    {ok, _} ->
      io:format("Consumer started successfully.~n");
    {error, Reason} ->
      io:format("Failed to start consumer: ~p~n", [Reason]),
      {error, Reason}
  end,
  % Start the publisher
  case publisher:start_link() of
    {ok, _} ->
      io:format("Publisher started successfully.~n"),
      {ok, self()};
    {error, Error} ->
      io:format("Failed to start publisher: ~p~n", [Error]),
      {error, Error}
  end.

stop(_State) ->
  ok.

%% internal functions
