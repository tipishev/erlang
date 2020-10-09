-module(lib_misc).
-export([unconsult/2, file_size_and_type/1]).

-include_lib("/usr/lib/erlang/lib/kernel-5.4.1/include/file.hrl").

unconsult(Filename, TermsList) ->
    {ok, F} = file:open(Filename, write),
    lists:foreach(fun(Term) -> io:format(F, "~p.~n", [Term]) end, TermsList),
    file:close(F).

file_size_and_type(File) ->
    case file:read_file_info(File) of
        {ok, Facts} ->
            {Facts#file_info.type, Facts#file_info.size};
        _ ->
            error
    end.



