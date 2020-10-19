-module(socket_examples).

% HTTP fetch example
-export([nano_get_url/0, nano_get_url/1]).

% client/server
-export([start_nano_server/0, nano_client_eval/1]).

nano_get_url() ->
    nano_get_url("bash.org").

nano_get_url(Host) ->
    % Any = gen_tcp:connect(Host, 80,[binary, {packet, 0}]),
    % io:format("~p~n", [Any]).
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket, _ReceivedSoFar=[]).

receive_data(Socket, ReceivedSoFar) ->
    receive
        {tcp, Socket, BinFragment} ->
            receive_data(Socket, [BinFragment|ReceivedSoFar]);
        {tcp_closed, Socket} ->
            % [second, first] -> [first, second] for efficiency
            TotalResponse = lists:reverse(ReceivedSoFar),
            list_to_binary(TotalResponse)
    end.

start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),  % to avoid receiving more connections
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [Str]),
            Reply = Str,  % just an echo
            io:format("Server replying ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345,
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.

