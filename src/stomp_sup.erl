-module(stomp_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [stomp_spec()],
    {ok, {{one_for_one, 1, 5}, Procs}}.

%% Private

stomp_spec() ->
    #{id       => stomp_proc,
      start    => {stomp_worker, start_link, []},
      restart  => permanent,
      shutdown => 5000,
      type     => worker,
      module   => [stomp_worker]}.
