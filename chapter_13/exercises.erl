-module(exercises).
-import(lib_misc, [on_exit/2]).
-compile(export_all).

ex1() ->
    my_spawn(exercises, red_shirt, []).

ex2() ->
    statistics(wall_clock),
    Pid = spawn(exercises, red_shirt, []),
    on_exit(Pid, report_time),
    Pid.

report_time(_Why) ->
  {_, Time} = statistics(wall_clock),
  io:format("it lived for ~p ms~n", [Time]).


        
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
