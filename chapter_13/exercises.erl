-module(exercises).
-compile(export_all).

run() ->
    my_spawn(exercises, red_shirt, []).

print(S) -> io:format("~p~n", [S]).

my_spawn(Mod, Func, Args) ->
    statistics(wall_clock),
    Pid = spawn(Mod, Func, Args),

    spawn(
      fun() ->
          Ref = monitor(process, Pid),
          receive
              {'DOWN', Ref, process, Pid, _Why} ->
                  {_, Time} = statistics(wall_clock),
                  io:format("it lived for ~p ms~n", [Time])
          end
      end
    ),

    Pid.

red_shirt() ->
    receive
        bullet -> error('I am dead lol');
        _ ->
            print("still alive"),
            red_shirt()
    end.
