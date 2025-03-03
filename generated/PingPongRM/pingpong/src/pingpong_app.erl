%%%-------------------------------------------------------------------
%% @doc pingpong public API
%% @end
%%%-------------------------------------------------------------------

-module(pingpong_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
%%    {ok, BobPid} = bob:start_link(),
    {ok, BobPid} = bob_alt:start_link(),
    {ok, _} = alice_alt:start_link(BobPid),

%%    {ok, Pid} = gen_bob:start_link(bob_alt, [initial_args]),
%%    register(bob_alt_pid, Pid),
%%    {ok, _} = alice_alt:start_link(Pid),
%%    {ok, _} = alice:start_link(BobPid),

    pingpong_sup:start_link().


stop(_State) ->
    ok.
