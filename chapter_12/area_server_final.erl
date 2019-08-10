-module(area_server_final).
-export([loop/0, start/0, area/2]).

start() -> spawn(area_server_final, loop, []).

area(Pid, What) ->
    rpc(Pid, What).

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
