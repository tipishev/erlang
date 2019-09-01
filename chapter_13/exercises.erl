-module(exercises).
-export([my_spawn/3]).

my_spawn(Mod, Func, Args) ->
    spawn(Mod, Func, Args).

red_shirt(Any) ->
    list_to_atom(Any).
