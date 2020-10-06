-module(mod_math).
-export([run/3]).

% as specified in the service section of config
run(MM, ArgC, ArgS) ->
    io:format("mod_math:run_starting~n"
              "ArgC = ~p ArgS=~p~n", [ArgC, ArgS]),
    loop(MM). % ArgC and ArgS are ignored

loop(MM) ->
    receive
        {chan, MM, {factorial, N}} ->
            MM ! {send, fac(N)},
            loop(MM);
        {chan, MM, {fibonacci, N}} ->
            MM ! {send, fib(N)},
            loop(MM);
        {chan_closed, MM} ->
            io:format("mod_math stopping"),
            exit(normal)  % the standard way to exit
    end.

fac(0) -> 1;
fac(N) -> N*fac(N-1).

fib(1) -> 1;
fib(2) -> 1;
fib(N) -> fib(N-1) + fib(N-2).


