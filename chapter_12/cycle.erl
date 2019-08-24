-module(cycle).
-compile(export_all).

start() ->
    spawn(?MODULE, loop, [42]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

% spawner(Head, N, N) ->
%     spawn(cycle, relay, [Head]);
% spawner(_, I, N) ->
%     spawn(cycle, relay, [self()]).


relay(Parent) ->
    receive
        Msg ->
            io:format("~p: 'got ~p, relaying to ~p'~n", 
                      [self(), Msg, Parent]),
            Parent ! Msg,
            relay(Parent)
     end.

head() ->
    receive
        Msg ->
            io:format("~p (head): 'got ~p'~n", 
                      [self(), Msg]),
            head()
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
