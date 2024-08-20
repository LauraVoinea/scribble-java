%%%-------------------------------------------------------------------
%% @doc pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(pingpong_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, BobPid} = bob:start_link(),
    {ok, _} = alice_alt:start_link(BobPid),
%%    {ok, _} = alice:start_link(BobPid),

    pingpong_sup:start_link().


stop(_State) ->
    ok.
