-module(hello2).
-export([hello/1]).

hello(Name) ->
    io:format("Hey ~s!~n", [Name]).

% 3> c(hello2).
% {ok,hello2}
% 4> hello2:hello("Joe").
% Hello Joe!
% ok
