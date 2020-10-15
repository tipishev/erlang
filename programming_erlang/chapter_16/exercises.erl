-module(exercises).

% user interface
-export([ex1/0, ex2/0, ex3/0, ex4/0, ex5/0]).

% local MFA export
-export([loop/1]).


-import(filelib, [last_modified/1]).
-import(string, [to_lower/1]).

-import(lib_find, [files/3]).

-compile(export_all).

ex1() -> check_recompile(?MODULE).
ex2() -> file_md5("lib_find.erl").
ex3() -> big_file_md5("/home/user/Downloads/archlinux-2020.10.01-x86_64.iso").
ex4() -> find_dupes(".").
ex5() ->
    Pid = start(),
    Pid ! "test",
    Pid ! "another".

%%% underhood


% get_md5(Filename) -> Filename.

start() ->
    spawn(?MODULE, loop, [#{}]).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop(Cache) ->
    receive
        {md5_request, Filename} ->
            Result = file_md5(Filename),  % TODO relay error
            io:format("Result: ~p~n", [Result]),
            loop(Cache);
        Any ->
            io:format("Received:~p~ncache:~p~n", [Any, Cache]),
            loop(Cache)
    end.


find_dupes(Dir) ->
    Filenames = files(Dir, "*.beam", true),
    FilesAndMd5s = lists:map(fun(Filename) -> {Filename, file_md5(Filename)} end, Filenames),
    Siblings = find_siblings(FilesAndMd5s, _Siblings=#{}),
    maps:filter(fun (_, Dupes) -> length(Dupes) > 1 end, Siblings).

find_siblings([{Filename, Md5}|Rest], Siblings) ->
    Dupes = maps:get(Md5, Siblings, []),
    UpdatedSiblings = maps:put(Md5, [Filename | Dupes], Siblings),
    find_siblings(Rest, UpdatedSiblings);
find_siblings([], Siblings) -> Siblings.

% ex4() -> count_characters("kalendula").
% count_characters(Str) ->
%    count_characters(Str, #{}).

% count_characters([Char|Rest], Counts) ->
%    Count = maps:get(Char, Counts, 0),
%    count_characters(Rest, maps:put(Char, Count + 1, Counts));

% count_characters([], X) ->
%         X.

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
