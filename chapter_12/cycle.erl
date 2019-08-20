-module(cycle).
-export([start/1, transmit/2, relay/0]).

start(M) ->
    % hm... chicken and egg with Next
    RPid = spawn(cycle, relay, []),
    spawn(cycle, transmit, [RPid, M]).

transmit(Next, M) ->
    receive
        MessageNum when MessageNum < M ->
            Next ! MessageNum + 1,
            transmit(Next, M);
        MessageNum when MessageNum == M ->
            io:format("done~n"),
            transmit(Next, M)
    end.

relay() ->
    receive
        % Message -> Next ! Message
        Msg ->
            io:format("relaying message ~p~n", [Msg]),
            relay()
    end.

