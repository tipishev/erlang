-module(exercises).

-export([ex1/0]).

ex1() -> check_recompile(?MODULE).

check_recompile(Module) ->
    ErlFile = io:format("~p.erl~n", [Module]),
    BeamFile = io:format("~p.beam~n", [Module]),
    filelib:last_modified(ErlFile).



