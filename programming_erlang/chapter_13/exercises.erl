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
    pacemake_spawn(pacemade, ?MODULE, pingus, [5000]).

ex5() ->
    io:format("Spawned Potus (~p), Flotus (~p), and Scotus (~p)~n",
              [pacemake_spawn(Name, ?MODULE, pingus, [5000])
               || Name <- [potus, flotus, scotus] ]).

ex6() ->
    Pid = group_spawn(),
    print("made a group spawn"),
    print(Pid),
    on_exit(Pid, fun(_Why) -> ex6() end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

group_spawn() ->
    spawn (
      fun () ->

        spawn_link(?MODULE, pingus, [5000]),
        spawn_link(?MODULE, pingus, [5000]),

        % to never exit
        receive
            after
                infinity -> true
        end

      end
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacemake_spawn(Name, Module, Function, Args) ->
    spawn(
      fun() ->
        MonitorRef = monitor(process, PayloadPid = spawn(Module, Function, Args)),
        register(Name, PayloadPid),
        receive
            {'DOWN', MonitorRef, process, PayloadPid, _Why} ->
                pacemake_spawn(Name, Module, Function, Args)
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
