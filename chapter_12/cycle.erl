-module(cycle).
-compile(export_all).

start() ->
    spawn(?MODULE, loop, [42]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop(X) ->
    receive
        Any ->
            io:format("received: ~p~n", [Any]),
            loop(X)
    end.

repeat(F, N) -> for(1, N, F).
for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].
