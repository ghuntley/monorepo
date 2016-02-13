-module(stomp_worker).
-behaviour(gen_server).

%% API.
-export([start_link/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-include("stomp.hrl").

%% State of a stomp_worker
-record(state, {connection    :: port(),
                next_sub      :: sub_id(),
                subscriptions :: #{ destination() => sub_id() },
                subscribers   :: #{ destination() => pid() }
               }).
-type state() :: #state{}.

%% API implementation

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    register(?MODULE, Pid),
    {ok, Pid}.

%% gen_server implementation

-spec init(any()) -> {ok, state()}.
init(_Args) ->
    %% Fetch configuration from app config
    {ok, Host} = application:get_env(stomp, host),
    Port = application:get_env(stomp, port, 61613),
    Login = application:get_env(stomp, login),
    Pass = application:get_env(stomp, passcode),

    %% Catch exit signals from linked processes (subscribers dying)
    process_flag(trap_exit, true),

    %% Establish connection
    {ok, Conn} = connect(Host, Port, Login, Pass),

    {ok, #state{connection = Conn,
                next_sub   = 0,
                subscriptions = #{},
                subscribers = #{}}}.

%% Handle subscription calls
handle_call({subscribe, Dest, Ack}, From, State) ->
    %% Subscribe to new destination
    SubId = State#state.next_sub,
    ok = subscribe(State#state.connection, SubId, Dest, Ack),

    %% Add subscription and subscriber to state
    Subscriptions = maps:put(SubId, Dest, State#state.subscriptions),
    Subscribers = maps:put(SubId, From, State#state.subscribers),
    NextSub = SubId + 1,
    NewState = State#state{subscriptions = Subscriptions,
                           subscribers = Subscribers,
                           next_sub = NextSub },

    {reply, {ok, SubId}, NewState};
handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

handle_info({tcp, Conn, Frame}, State#state{connection = Conn}) ->
    handle_frame(Frame, State);
handle_info(_Msg, State) ->
    {noreply, State}.

%% Unused gen_server callbacks

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private functions

-spec connect(list(), integer(), any(), any()) -> {ok, port()}.
connect(Host, Port, Login, Pass) ->
    %% STOMP CONNECT frame
    Connect = connect_frame(Host, Login, Pass),

    %% TODO: Configurable buffer size
    %% Frames larger than the user-level buffer will be truncated, so it should
    %% never be smaller than the largest expected messages.
    {ok, Socket} = gen_tcp:connect(Host, Port, [binary,
                                                {packet, line},
                                                {line_delimiter, $\0},
                                                {buffer, 262144}]),

    ok = gen_tcp:send(Socket, Connect),
    {ok, Socket}.

-spec subscribe(port(), sub_id(), destination(), ack_mode()) -> ok.
subscribe(Socket, Id, Queue, Ack) ->
    {ok, SubscribeFrame} = subscribe_frame(Id, Queue, Ack),
    gen_tcp:send(Socket, SubscribeFrame).

%%% Parsing STOMP frames

handle_frame(<<"MESSAGE", "\n", Frame/binary>>,
             #state{subscribers = Subscribers,
                    subscriptions = Subscriptions}) ->
    
    {noreply, State};
handle_frame(Frame, State) ->
    io:format("Received unknown frame ~p", [Frame]),
    {noreply, State}.


%% Parse out headers into a map
-spec parse_headers(binary()) -> headers().
parse_headers(HeadersBin) ->
    Headers = binary:split(HeadersBin, <<"\n">>, [global]),
    ToPairs = fun(H, M) -> [K,V | []] = binary:split(H, <<":">>),
                           maps:put(K, V, M)
              end,
    {ok, lists:mapfoldl(ToPairs, #{}, Headers)}.

%%% Making STOMP protocol frames

%% Format a header
-spec format_header({binary(), binary()}) -> binary().
format_header({Key, Val}) ->
    <<Key, ":", Val, "\n">>.

%% Build a single STOMP frame
-spec make_frame(binary(),
                 list({binary(), binary()}),
                 binary())
                -> {ok, iolist()}.
make_frame(Command, HeaderMap, Body) ->
    Headers = lists:map(fun format_header/1, HeaderMap),
    Frame = [Command, <<"\n">>, Headers, <<"\n">>, Body, <<0>>],
    {ok, Frame}.

%%% Default frames

-spec connect_frame(list(), any(), any()) -> iolist().
connect_frame(Host, {ok, Login}, {ok, Pass}) ->
    make_frame(<<"CONNECT">>,
               [{"accept-version", "1.2"},
                {"host", Host},
                {"login", Login},
                {"passcode", Pass},
                {"heart-beat", "0,5000"}],
               []);
connect_frame(Host, _Login, _Pass) ->
    make_frame(<<"CONNECT">>,
               [{"accept-version", "1.2"},
                {"host", Host},
                %% Expect a server heartbeat every 5 seconds, let the server
                %% expect one every 10. We don't actually check this and just
                %% echo server heartbeats.
                %% TODO: For now the server is told not to expect replies due to
                %% a weird behaviour.
                {"heart-beat", "0,5000"}],
               []).


-spec subscribe_frame(sub_id(), destination(), ack_mode()) -> iolist().
subscribe_frame(Id, Queue, Ack) ->
    make_frame(<<"SUBSCRIBE">>,
               [{"id", integer_to_binary(Id)},
                {"destination", Queue},
                {"ack", ack_mode_to_binary(Ack)}],
               []).

-spec ack_mode_to_binary(ack_mode()) -> binary().
ack_mode_to_binary(AckMode) ->
    case AckMode of
        auto              -> <<"auto">>;
        client            -> <<"client">>;
        client_individual -> <<"client-individual">>
    end.

%% -spec ack_frame(binary()) -> iolist().
%% ack_frame(MessageID) ->
%%     make_frame(<<"ACK">>,
%%                [{"id", MessageID}],
%%                []).
