-module(email).
-export([
    start/0,
    % stop/0
    list/1
    % get/2
    % send/3
]).

-record(message, {id, from, to, content}).
% -record(request, {operation, args}).


%%% Exercise 17.5

%% Start email server on port 8008

-spec start() -> no_return().

start() ->
    start_tcp_listener(_PortNumber=8008).

-spec start_tcp_listener(PortNumber) -> ParallelServerPid
                                          when
      PortNumber :: inet:port_number(),
      ParallelServerPid :: pid().

%%% Server

start_tcp_listener(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> run_parallel_server(Listen) end).


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

handle({list, Username}) ->
    {ok, list_messages(Username)}.

-spec list_messages(Username) -> Emails when
      Username :: string(),
      Emails :: [term()].

list_messages(Username) ->
    Messages = [
        #message{id=1, from="bob", to="alice", content="Hi!"},
        #message{id=2, from="alice", to="bob", content="Hullo!"}
    ],
    lists:filter(fun(#message{to=To}) -> To =:= Username end, Messages).

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
            Val = binary_to_term(Bin),
            io:format("Received: ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.
