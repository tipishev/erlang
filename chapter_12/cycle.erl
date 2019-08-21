-module(cycle).
-export([
     start/2,
     transmit/2,
     relay/0,
     repeat/2
]).

start(N, M) ->
    % statistics(runtime),
    % statistics(wall_clock),

    % hm... chicken and egg with Next
    RPid = spawn(cycle, relay, []),
    spawn(cycle, transmit, [RPid, M]).

transmit(Next, M) ->
    receive
        die -> void;
        MessageNum when MessageNum < M ->
            Next ! MessageNum + 1,
            transmit(Next, M);
        MessageNum when MessageNum == M ->
            io:format("done~n"),
            transmit(Next, M)
    end.

relay() ->
    receive
        die -> void;
        % Message -> Next ! Message
        Msg ->
            io:format("relaying message ~p~n", [Msg]),
            relay()
    end.

repeat(F, N) -> for(1, N, F).
for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].
