-module(email).
-export([
    start/0
    % ,stop/0
    ,list/1
    % ,get/2
    % ,send/3
]).

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


-spec email_loop(Socket) -> no_return()
                                          when
      Socket :: gen_tcp:socket().

email_loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Loop socket received~p~n", [Bin]),
            Val = binary_to_term(Bin),
            Response = term_to_binary(Val),
            gen_tcp:send(Socket, Response),
            email_loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Loop socket closed~n")
    end.

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



