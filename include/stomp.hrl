%% Client ack modes, refer to the STOMP protocol documentation
-type ack_mode() :: client | client_individual | auto.

%% Subscriptions are enumerated from 0
-type sub_id() :: integer().

%% Message IDs (for acknowledgements) are simple strings. They are
%% extracted from the 'ack' field of the header in client or client-individual
%% mode, and from the 'message-id' field in auto mode.
-type message_id() :: binary().

%% A destination can be a queue, or something else.
%% Example: <<"/queue/lizards">>
-type destination() :: binary().

%% A STOMP message as received from a queue subscription
-record(stomp_msg, { headers :: #{ binary() => binary() },
                     body    :: binary() }).
-type stomp_msg() :: #stomp_msg{}.
