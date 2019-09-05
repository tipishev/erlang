-module(lib_misc).
-export([on_exit/2, start/1, keep_alive/2]).

on_exit(Pid, Fun) ->
    spawn(
      fun() ->
              Ref = monitor(process, Pid),
              receive
                  {'DOWN', Ref, process, Pid, Why} -> Fun(Why)
              end
      end
    ).

% start the die-together group
start(Fs) ->
    spawn(fun() ->
              [spawn_link(F) || F <- Fs],
              receive
                  after
                      infinity -> true
              end
          end
).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).
