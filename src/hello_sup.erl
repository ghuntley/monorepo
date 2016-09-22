-module(hello_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%%% Module API

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%% Supervisor callbacks

init([]) ->
    Children = [hello_spec()],
    {ok, { {one_for_one, 5, 10}, Children}}.

%%% Private

hello_spec() ->
    #{id       => hello_server2,
      start    => {hello_server2, start_link, []},
      restart  => permanent,
      shutdown => 5000,
      type     => worker,
      module   => [hello_server2]}.
