-module(client).

-export([start/0]).

start() ->
    {ok, P} = python:start(),
    python:call(P, client, start, [self()]).
