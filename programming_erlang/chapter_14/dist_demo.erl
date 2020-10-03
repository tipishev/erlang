-module(dist_demo).

-export([rpc/4, start/1]).

start(Node) ->
    spawn(Node, fun() -> loop() end).

rpc(Pid, Module, Function, Arguments) ->
    Pid ! {rpc, self(), Module, Function, Arguments},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {rpc, Pid, Module, Function, Arguments} ->
            Pid ! {self(), (catch apply(Module, Function, Arguments))}
    end.
