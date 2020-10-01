-module(exercises).
-import(lib_misc, [on_exit/2]).

-compile(export_all).

ex1() ->
    RedShirt = spawn_with_post_mortem(?MODULE, red_shirt, []),
    RedShirt ! missfire,
    RedShirt ! bullet.

ex2() ->
    RedShirt = spawn(?MODULE, red_shirt, []),
    {TimeOfSpawn, _} = statistics(wall_clock),
    on_exit(RedShirt, fun(CauseOfDeath) ->
                              report_time(CauseOfDeath, TimeOfSpawn)
                      end),
    RedShirt ! missfire,
    RedShirt ! bullet.

ex3() ->
    RedShirt = spawn_with_timer(?MODULE, red_shirt, [], _TimeToDie=7000),
    RedShirt ! missfire.

ex4() ->
    pacemake_spawn(?MODULE, pingus, [5000]).

ex5() ->
    First = spawn(?MODULE, pingus, [5000]),
    Second = spawn(?MODULE, pingus, [5000]),
    Third = spawn(?MODULE, pingus, [5000]).


ex6() ->
    42.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacemake_spawn(Module, Function, Args) ->
    spawn(
      fun() ->
        MonitorRef = monitor(process, PayloadPid = spawn(Module, Function, Args)),
        register(pacemade, PayloadPid),
        receive
            {'DOWN', MonitorRef, process, PayloadPid, _Why} ->
                pacemake_spawn(Module, Function, Args)
        end
      end
    ).

pingus(Period) ->
    io:format("Ah-ah-ah-ah ~p staying alive.~n", [self()]),
    receive
        die ->
            print("Just a flesh wound."),
            void
    after Period ->
      pingus(Period)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report_time(CauseOfDeath, TimeOfSpawn) ->
  {TimeOfDeath, _} = statistics(wall_clock),
  io:format("it lived for ~p ms and died of ~p~n",
            [TimeOfDeath - TimeOfSpawn, CauseOfDeath]).

print(S) -> io:format("~p~n", [S]).


spawn_with_post_mortem(Mod, Func, Args) ->

    SpawnedProcess = spawn(Mod, Func, Args),
    {TimeOfSpawn, _} = statistics(wall_clock),

    spawn(
      fun() ->
          MonitorRef = monitor(process, SpawnedProcess),
          receive
              {'DOWN', MonitorRef, process, SpawnedProcess, {CauseOfDeath, _}} ->
                  {TimeOfDeath, _} = statistics(wall_clock),
                  io:format("it lived for ~p ms and died of ~p~n",
                            [TimeOfDeath - TimeOfSpawn, CauseOfDeath])
          end
      end
    ),

    SpawnedProcess.

spawn_with_timer(Mod, Func, Args, TimeToDie) ->
    link_diemer(Pid = spawn(Mod, Func, Args), TimeToDie),
    io:format("Will kill ~p in ~p ms~n",
               [Pid, TimeToDie]),
    Pid.

link_diemer(Pid, TimeToDie) ->
    spawn(

    fun() ->

        link(Pid),

        receive
        after TimeToDie ->
              print("My time has come."),
              exit(my_time_has_come)  % should kill the linked lh
        end

    end
    ).

red_shirt() ->
    receive
        bullet -> error('Just a flesh wound.');
        _ ->
            print("still alive :P" ),
            red_shirt()
    end.
