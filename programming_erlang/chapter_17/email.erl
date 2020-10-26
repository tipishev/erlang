-module(email).

%%% Exercise 17.5: a simple file-based email service

%% user interface exports
-export([
         start/0,
         % stop/0
         
         list/1,
         get/2
         % send/3
        ]).

%% types declarations
-type username() :: string().
-type message_id() :: non_neg_integer().
-type request() :: {list, Username :: username()} | {get, Username :: username(), MessageId :: message_id()}.
-type response() :: {ok, Result :: term()} |{error, Reason :: atom()}.

%% records declarations
-record(message, {id :: message_id(),
                  from :: username(),
                  to :: username(),
                  content :: term()}).
% -record(request, {operation, args}).



%% Start email server on port 8008

-spec start() -> no_return().

start() ->
    start_tcp_listener(_PortNumber=8008).

-spec start_tcp_listener(PortNumber) -> ok
                                          when
      PortNumber :: inet:port_number().

%%% Server

%% TODO spawn it for an easy kill
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

handle({get, Username, MessageId}) ->
    case get_message(Username, MessageId) of 
        not_found -> {error, not_found};
        Message -> {ok, Message}
    end.

-spec list_messages(Username) -> Messages when
      Username :: string(),
      Messages :: [#message{}].

%% always succeeds for any user
list_messages(Username) ->
    % TODO read from file
    Messages = [
                #message{id=1, from="bob", to="alice", content="Hi!"},
                #message{id=2, from="alice", to="bob", content="Hullo!"},
                #message{id=3, from="alice", to="bob", content="Did you get my previous message?"}
               ],
    % lists:filter(fun(#message{to=To}) -> To =:= Username end, Messages).
    [Message || Message=#message{to=To} <- Messages, To =:= Username].

-spec get_message(Username, MessageId) -> Message | not_found when
      Username :: string(),
      MessageId :: message_id(),
      Message :: #message{}.

get_message(Username, MessageId) ->
    UserMessages = list_messages(Username),
    get_message_by_id(UserMessages, MessageId).

-spec get_message_by_id(Messages, MessageId) -> Message | not_found
                                     when
      Messages :: [#message{}],
      MessageId :: message_id(),
      Message :: #message{}.

get_message_by_id([], _MessageId) -> not_found;
get_message_by_id([#message{id=MessageId}=Message|_T], MessageId) -> Message;
get_message_by_id([_H|T], MessageId) -> get_message_by_id(T, MessageId).


%%% Client
-spec list(Username) -> Messages
                          when
      Username :: string(),
      Messages :: [#message{}].

list(Username) ->
    {ok, Socket} = gen_tcp:connect("localhost", 8008, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary({list, Username})),
    receive
        {tcp, Socket, Bin} ->
            ok = gen_tcp:close(Socket),
            {ok, RawMessages} = binary_to_term(Bin),
            MessagesTable = tabulate(RawMessages),
            io:format(MessagesTable)  % TODO deduplicate
    end.

-spec get(Username, MessageId) -> Message | {error, Reason}
                                    when
      Username :: username(),
      MessageId :: message_id(),
      Message :: #message{},
      Reason :: not_found.

get(Username, MessageId) ->
    {ok, Socket} = gen_tcp:connect("localhost", 8008, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary({get, Username, MessageId})),
    receive
        {tcp, Socket, Bin} ->
            ok = gen_tcp:close(Socket),
            Result = binary_to_term(Bin),
            case Result of 
                {ok, RawMessage} ->
                    MessagesTable = tabulate([RawMessage]),
                    io:format(MessagesTable);  % TODO deduplicate
                {error, not_found} ->
                    io:format("No message with id ~p for ~p~n",
                              [MessageId, Username])
            end
    end.



%% convert messages to a nicely-printable io_list
-spec tabulate(Messages) -> FormattedMessages
                              when
      Messages :: [#message{}],
      FormattedMessages :: iolist().

tabulate(Messages) ->
    tabulate(Messages, []).

tabulate(Messages, []) -> tabulate(Messages, ["Id\tFrom\tContent\n"]);
tabulate([#message{id=Id, from=From, content=Content}|Tail], Acc) ->
    Row = io_lib:format("~p\t~p\t~p~n", [Id, From, Content]),
    tabulate(Tail, [Row|Acc]);
tabulate([], Acc) -> lists:reverse(Acc).


% -spec unconsult(File, Terms)

% unconsult(Filename, Terms) ->
%     {ok, File} = file:open(Filename, write),
%     lists:foreach(
%       fun(Term) -> io:format(File, "~p.~n", [Term]) end, Terms),
%     file:close(File).
