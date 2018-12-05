-module(launch).
-export([client/3, server/1]).
-export([get/1]).
-import(erlang, [set_cookie/2]).
-import(server, [start_link/0]).
-import(client, [start/1]).


client(Server, Node, Cookie) ->
    Path = "./client/",
    Pid = {Server, Node},
    set_cookie(node(), Cookie),
    {ok, P} = python:start([{cd, Path}]),
    python:call(P, client, start, [Pid]).


server(Cookie) ->
    set_cookie(node(), Cookie),
    {ok, Pid} = server:start_link(),
    register(server, Pid),
    io:format("~w~n", [Pid]),
    is_pid(server).

get(P) ->
    receive
	Message ->
	    python:call(P, client, message_received, [Message])
    end,
    ?MODULE:get(P).

