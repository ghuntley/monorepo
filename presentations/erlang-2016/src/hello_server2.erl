-module(hello_server2).
-behaviour(gen_server).
-compile(export_all).

%%% Start callback for supervisor
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% gen_server callbacks

init([]) ->
    {ok, sets:new()}.

handle_call({greet, Name}, _From, State) ->
    io:format("Hello ~s!~n", [Name]),
    NewState = sets:add_element(Name, State),
    {reply, ok, NewState};

handle_call({bye, Name}, _From, State) ->
    io:format("Goodbye ~s!~n", [Name]),
    NewState = sets:del_element(Name, State),
    {reply, ok, NewState}.

terminate(normal, State) ->
    [io:format("Goodbye ~s!~n", [Name]) || Name <- State],
    ok.

%%% Unused gen_server callbacks
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.
