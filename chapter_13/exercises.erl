-module(exercises).
-compile(export_all).

ex1() ->
    my_spawn(exercises, red_shirt, []).

ex2() ->
    statistics(wall_clock),
    Pid = spawn(exercises, red_shirt, []),
    % on_exit(Pid, report_time),
    lib_misc:on_exit(Pid, fun(_Why) -> report_time(_Why) end),
    Pid.

ex3() ->
    spawn_with_timeout(exercises, red_shirt, [], 3000).

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

spawn_with_time(Mod, Func, Args, Time) ->
    Pid = spawn(Mod, Func, Args).
    
diemer(Time) ->
    receive
    after Time ->
          print("My time has come..."),
          exit(my_time_has_come)
    end.

red_shirt() ->
    receive
        bullet -> error('I am dead lol');
        _ ->
            print("still alive"),
            red_shirt()
    end.
