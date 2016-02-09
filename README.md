STOMP on Erlang
===============

`stomp.erl` is a simple Erlang client for the [STOMP protocol][] in version 1.2.

Currently only subscribing to queues is supported.

It provides an application called `stomp` which takes configuration of the form:

```erlang
[{stomp, #{host     => "stomp-server.somedomain.sexy", % required
           port     => 61613,                          % optional
           login    => <<"someuser">>,                 % optional
           passcode => <<"hunter2>>,                   % optional
 }}].
```

## Types

The following types are used in `stomp.erl`, you can include them from
`stomp.hrl`:

```erlang
%% Client ack modes, refer to the STOMP protocol documentation
-type ack_mode() :: client | client_individual | auto.

%% Subscriptions are enumerated from 0
-type sub_id() :: integer().

%% Message IDs (for acknowledgements) are simple strings. They are
%% extracted from the 'ack' field of the header in client or client-individual
%% mode, and from the 'message-id' field in auto mode.
-type message_id() :: binary().

%% A STOMP message as received from a queue subscription
-record(stomp_msg, { headers :: #{ binary() => binary() },
                     body    :: binary() }.
-type stomp_msg() :: #stomp_msg{}.
```

Once the application starts it will register a process under the name
`stomp_worker` and expose the following API:

## Subscribing to a queue

```erlang
%% Subscribe to a destination, receive the subscription ID
-spec subscribe(binary(),   % Destination (e.g. <<"/queue/lizards">>)
                ack_mode(), % Client-acknowledgement mode
                -> {ok, sub_id()}.
```

This synchronous call subscribes to a message queue. The `stomp_worker` will
link itself to the caller and forward received messages as
`{msg, sub_id(), stomp_msg()}`.

Depending on the acknowledgement mode specified on connecting, the subscriber
may have to acknowledge receival of messages.

## Acknowledging messages

```erlang
%% Acknowledge a message ID.
%% This is not required in auto mode. In client mode it will acknowledge the
%% received messages up to the ID specified. In client-individual mode every
%% single message has to be acknowledged.
-spec ack(sub_id(), message_id()) -> ok.

%% Explicitly "unacknowledge" a message
-spec nack(sub_id(), message_id()) -> ok.
```

Both of these calls are asynchronous and will return immediately. Note that in
the case of the `stomp_worker` crashing before a message acknowledgement is
handled, the message *may* be delivered again. Your consumer needs to be able to
handle this.

[STOMP protocol]: https://stomp.github.io/stomp-specification-1.2.html
