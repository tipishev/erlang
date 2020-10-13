-module(exercises).

-export([ex1/0, ex2/0, ex3/0]).
-import(filelib, [last_modified/1]).
-import(string, [to_lower/1]).

ex1() -> check_recompile(?MODULE).
ex2() -> file_md5("lib_find.erl").
ex3() -> big_file_md5("/home/user/Downloads/archlinux-2020.10.01-x86_64.iso").

%% underhood

big_file_md5(Filename) ->
    Context = erlang:md5_init(),
    {ok, File} = file:open(Filename, [read, binary, raw]),
    md5_in_chunks(Context, File, _ChunkSize=4096, _Offset=0).

md5_in_chunks(Context, File, ChunkSize, Offset) ->
    ChunkOrEof = file:pread(File, Offset, ChunkSize),
    case ChunkOrEof of
        {ok, Data} ->
            NewContext = erlang:md5_update(Context, Data),
            md5_in_chunks(NewContext, File, ChunkSize, Offset + ChunkSize);
        eof ->
            <<Int:128>> = erlang:md5_final(Context),
            to_lower(integer_to_list(Int, 16))
    end.



file_md5(Filename) ->
    case file:read_file(Filename) of
        {ok, File} ->
            <<Int:128>> = erlang:md5(File),
            to_lower(integer_to_list(Int, 16));
        {error, Reason} ->
            {error, Reason}
    end.

check_recompile(Module) ->
    Erl = concat(Module, ".erl"),
    Beam = concat(Module, ".beam"),
    % TODO handle non-existent files: last_modified returns 0
    last_modified(Erl) > last_modified(Beam).

concat(A, B) when is_atom(A) ->
    A1 = erlang:atom_to_list(A),
    concat(A1, B);
concat(A, B) when is_list(A), is_list(B) ->
    unicode:characters_to_list([A, B]).
