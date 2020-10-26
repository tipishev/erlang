-module(email).

%%% Exercise 17.5: a simple file-based email service

%% user interface exports
-export([
         start/0,
         % stop/0
         
         list/1,
         get/2,
         send/3
        ]).

%% helpers
-export([create_mbox/0]).

%% records declarations
-record(message, {id :: message_id(),
                  from :: username(),
                  to :: username(),
                  content :: term()}).

%% types declarations
-type username() :: string().
-type message_id() :: non_neg_integer().
-type request() :: {list, Username :: username()}
                 | {get, Username :: username(), MessageId :: message_id()}
                 | {send, From :: username(), To :: username(), Message :: message()}.
-type response() :: {ok, Result :: term()} | {error, Reason :: atom()}.
-type message() :: #message{}.

%%% Server

%% Start email server on port 8008

-spec start() -> no_return().

start() ->
    start_tcp_listener(_PortNumber=8008).

%% TODO spawn it for an easy kill
-spec start_tcp_listener(PortNumber) -> ok
                                          when
      PortNumber :: inet:port_number().

start_tcp_listener(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> run_parallel_server(Listen) end),
    ok.


-spec run_parallel_server(Listen) -> no_return()
                                       when
      Listen :: port().

run_parallel_server(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> run_parallel_server(Listen) end),
    email_loop(Socket).


-spec email_loop(Socket) -> ok
                              when
      Socket :: gen_tcp:socket().

email_loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Request = binary_to_term(Bin),
            Response = handle(Request),
            EncodedResponse = term_to_binary(Response),
            gen_tcp:send(Socket, EncodedResponse),
            email_loop(Socket);
        {tcp_closed, Socket} ->
            ok
    end.

-spec handle(Request) -> Response when
      Request :: request(),
      Response :: response().

%% requests handler, pattern match different requests
handle(_Request = {list, Username}) ->
    {ok, list_messages(Username)};  % always succeeds

handle(_Request = {get, Username, MessageId}) ->
    case get_message(Username, MessageId) of 
        not_found -> {error, not_found};
        Message -> {ok, Message}
    end;

handle(_Request = {send, To, From, Message}) ->
    MessageId = send_message(From, To, Message),
    {ok, MessageId}.


-spec list_messages(Username) -> Messages when
      Username :: string(),
      Messages :: [message()].


%% always succeeds for any user
list_messages(Username) ->
    {ok, Messages} = file:consult("mbox"),
    % lists:filter(fun(#message{to=To}) -> To =:= Username end, Messages).
    [Message || Message=#message{to=To} <- Messages, To =:= Username].

-spec get_message(Username, MessageId) -> Message | not_found when
      Username :: string(),
      MessageId :: message_id(),
      Message :: message().

get_message(Username, MessageId) ->
    UserMessages = list_messages(Username),
    lists:keyfind(MessageId, 2, UserMessages).

-spec send_message(From, To, Message) -> MessageId when
      From :: username(),
      To :: username(),
      Message :: message(),
      MessageId :: message_id().

send_message(From, To, Message) ->
    {ok, ExistingMessages} = file:consult("mbox"),
    MessageId = length(ExistingMessages) + 1,
    ok = unconsult("mbox", lists:reverse([#message{id = MessageId,
                                                   to = To,
                                                   from = From,
                                                   content = Message}
                                          |ExistingMessages])),
    MessageId.


%%% Client
-spec list(Username) -> Messages
                          when
      Username :: string(),
      Messages :: [message()].

list(Username) ->
    RawMessages = rpc({list, Username}),
    MessagesTable = tabulate(RawMessages),
    io:format(MessagesTable).  % TODO deduplicate

-spec get(Username, MessageId) -> ok | {error, Reason}
                                    when
      Username :: username(),
      MessageId :: message_id(),
      % Format :: io:format(),
      Reason :: not_found.

get(Username, MessageId) ->
    RawMessage = rpc({get, Username, MessageId}),
    MessagesTable = tabulate([RawMessage]),
    io:format(MessagesTable).  % TODO deduplicate

-spec send(To, From, Message) -> {ok, MessageId}
                                   when
      To :: username(),
      From :: username(),
      Message :: message(),
      MessageId :: message_id().

send(To, From, Message) ->
    MessageId = rpc({send, To, From, Message}),
    {ok, MessageId}.


%%% Helpers

-spec rpc(Operation) -> Response when
      Operation :: tuple(),
      Response :: term().

% TODO encryption, custom encode/decode functions
rpc(Operation) ->
    {ok, Socket} = gen_tcp:connect("localhost", 8008, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Operation)),
    receive
        {tcp, Socket, Bin} ->
            ok, gen_tcp:close(Socket),
            {ok, Payload} = binary_to_term(Bin),
            Payload
    end.

-spec create_mbox() -> ok.
create_mbox() ->
    Messages = [
                #message{id=1, from="bob", to="alice", content="Hi!"},
                #message{id=2, from="alice", to="bob", content="Hullo!"},
                #message{id=3, from="alice", to="bob",
                         content="Did you get my previous message?"}
               ],
    unconsult("mbox", Messages).

%% convert messages to a nicely-printable io_list

-spec tabulate(Messages) -> FormattedMessages
                              when
      Messages :: [#message{}],
      FormattedMessages :: iolist().

tabulate(Messages) ->
    tabulate(Messages, []).

-spec tabulate(Messages, Acc) -> FormattedMessages
                              when
      Messages :: [#message{}],
      Acc :: iolist(),
      FormattedMessages :: iolist().

tabulate(Messages, []) -> tabulate(Messages, ["Id\tFrom\tContent\n"]);
tabulate([#message{id=Id, from=From, content=Content}|Tail], Acc) ->
    Row = io_lib:format("~p\t~p\t~p~n", [Id, From, Content]),
    tabulate(Tail, [Row|Acc]);
tabulate([], Acc) -> lists:reverse(Acc).

%% file operations

-spec unconsult(Filename, Terms) -> ok 
                                      when
      Filename :: string(),
      Terms :: [term()].


unconsult(Filename, Terms) ->
    {ok, File} = file:open(Filename, write),
    lists:foreach(
      fun(Term) -> io:format(File, "~p.~n", [Term]) end, Terms),
    file:close(File).
