-module(lib_misc).

-export([unconsult/2]).


unconsult(Filename, TermsList) ->
    {ok, F} = file:open(Filename, write),
    lists:foreach(fun(Term) -> io:format(F, "~p.~n", [Term]) end, TermsList),
    file:close(F).
