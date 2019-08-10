-module(area_server_final).
-export([loop/0, rpc/2]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Height}} ->
            From ! {self(), Width * Height},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, {circle, Radius}} ->
            From ! {self(), 3.141592 * Radius * Radius},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}}
    end.
