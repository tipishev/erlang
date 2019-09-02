-module(exercises).
-compile(export_all).

run() ->
    my_spawn(exercises, red_shirt, []).

print(S) -> io:format("~p~n", [S]).

my_spawn(Mod, Func, Args) ->
    statistics(runtime),
    Pid = spawn(Mod, Func, Args),
    {_, Time} = statistics(runtime),
    io:format("it lived for ~p ms~n", [Time]),
    Pid.

red_shirt() ->
    receive
        bullet -> error('I am dead lol');
        _ ->
            print("still alive"),
            red_shirt()
    end.
