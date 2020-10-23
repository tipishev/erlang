-module(email).

%%% Exercise 17.5: a simple file-based email service

%% user interface
-export([
         start/0,
         % stop/0
         
         list/1
         % get/2
         % send/3
        ]).

%% records declarations
-type username() :: string().
-record(message, {id :: non_neg_integer(),
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
            {ok, Response} = handle(Request),
            EncodedResponse = term_to_binary(Response),
            gen_tcp:send(Socket, EncodedResponse),
            email_loop(Socket);
        {tcp_closed, Socket} ->
            ok
    end.

-type request() :: {list, Username :: string()}.
-type response() :: {ok, Result :: term()}.
-spec handle(Request) -> Response when
      Request :: request(),
      Response :: response().

%% requests handler, for now only list
handle({list, Username}) ->
    {ok, list_messages(Username)}.

-spec list_messages(Username) -> Emails when
      Username :: string(),
      Emails :: [#message{}].

list_messages(Username) ->
    % TODO read from file
    Messages = [
                #message{id=1, from="bob", to="alice", content="Hi!"},
                #message{id=2, from="alice", to="bob", content="Hullo!"},
                #message{id=3, from="alice", to="bob", content="Did you get my previous message?"}
               ],
    % lists:filter(fun(#message{to=To}) -> To =:= Username end, Messages).
    [Message || Message=#message{to=To} <- Messages, To =:= Username].

%%% Client
-spec list(Username) -> Emails
                          when
      Username :: string(),
      Emails :: [term()].

list(Username) ->
    {ok, Socket} = gen_tcp:connect("localhost", 8008, [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary({list, Username})),
    receive
        {tcp, Socket, Bin} ->
            ok = gen_tcp:close(Socket),
            RawMessages = binary_to_term(Bin),
            MessagesTable = tabulate(RawMessages),
            io:format(MessagesTable)
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
