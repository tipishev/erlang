-module(exercises).
-compile(export_all).

ex1() ->
    spawn_with_post_mortem(?MODULE, red_shirt, []).

ex2() ->
    statistics(wall_clock),
    Pid = spawn(?MODULE, red_shirt, []),
    % on_exit(Pid, report_time),
    lib_misc:on_exit(Pid, fun(_Why) -> report_time(_Why) end),
    Pid.

ex3() ->
    spawn_with_time(?MODULE, red_shirt, [], 5000).

ex4() ->
    Period = 5000,
    pacemake(pingus, [Period]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pacemake(Fun, Args) ->
    spawn(?MODULE, keep_alive, [Fun, Args]).

% TODO parametrize module?
keep_alive(Fun, Args) ->
    case whereis(Fun) of
        undefined ->
            Pid = spawn(?MODULE, Fun, Args),
            register(Fun, Pid);  % no atomic clash, neat
        Pid -> Pid
    end,
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Why} ->
            io:format("Reviving after ~p suddenly ~p~n", [Pid, Why]),
            NewPid = spawn(?MODULE, Fun, Args),
            register(Fun, NewPid),  % no atomic clash, neat
            keep_alive(Fun, Args)
    end.
            
pingus(Period) ->
    print("Ah-ah-ah-ah staying alive"),
    receive
        die ->
            print("Just a flesh wound"),
            void;
         _Any ->
          pingus(Period)
    after Period ->
      pingus(Period)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report_time(_Why) ->
  {_, Time} = statistics(wall_clock),
  io:format("it lived for ~p ms~n", [Time]).

print(S) -> io:format("~p~n", [S]).


spawn_with_post_mortem(Mod, Func, Args) ->
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
    link_diemer(Pid = spawn(Mod, Func, Args), Time),
    Pid.

link_diemer(Pid, Time) ->
    spawn(

    fun() ->

        link(Pid),

        receive
        after Time ->
              print("My time has come..."),
              exit(my_time_has_come)
        end

    end
    ).

red_shirt() ->
    receive
        bullet -> error('I am dead lol');
        _ ->
            print("still alive"),
            red_shirt()
    end.
