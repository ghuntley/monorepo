-module(hello_server).
-export([start_server/0, server/0]).

start_server() ->
    spawn(fun() -> server() end).

server() ->
    receive
        {greet, Name} ->
            io:format("Hello ~s!~n", [Name]),
            hello_server:server()
    end.
