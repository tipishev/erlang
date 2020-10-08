-module(socket_examples).
-export([nano_get_url/0, nano_get_url/1]).

nano_get_url() ->
    nano_get_url("erlang.se").

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
