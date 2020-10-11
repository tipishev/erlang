-module(exercises).

-export([ex1/0, ex2/0]).
-import(filelib, [last_modified/1]).

ex1() -> check_recompile(?MODULE).
ex2() -> file_md5("lib_find.erl").

%% underhood

file_md5(Filename) ->
    case file:read_file(Filename) of
        {ok, File} ->
            binary_to_list(erlang:md5(File));
        {error, Reason} ->
            {error, Reason}
    end.

check_recompile(Module) ->
    Erl = concat(Module, ".erl"),
    Beam = concat(Module, ".beam"),
    last_modified(Erl) > last_modified(Beam).

concat(A, B) when is_atom(A) ->
    A1 = erlang:atom_to_list(A),
    concat(A1, B);
concat(A, B) when is_list(A), is_list(B) ->
    unicode:characters_to_list([A, B]).





