-module(exercise1).
-export([start/2, waiter/0]).

start(AnAtom, Fun) ->
    Pid = spawn(fun() -> Fun() end),
    register(AnAtom, Pid).

waiter() -> 
    receive
        stop ->
            void
    end.
