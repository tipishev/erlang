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
    start_md5(),
    Md5 = get_md5("lib_find.erl"),
    Md5 = get_md5("lib_find.erl"),
    print(Md5),
    stop_md5().


%%% underhood

start_md5() ->
    register(md5, spawn(?MODULE, loop, [#{}])).

stop_md5() ->
    md5 ! stop.  % exit(normal) auto-unregisters.

get_md5(Filename) ->
    Pid = whereis(md5),
    rpc(Pid, Filename).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response;
        Any ->
            io:format("received unexpected~p~n", [Any])
    after 1000 ->
        timeout
    end.

loop(Cache) ->
    receive
        {From, Filename} ->
            LastModified = filelib:last_modified(Filename),
            case maps:find(Filename, Cache) of
                {ok, {LastModified, Md5}} ->  % send cached
                    print("cached out"),
                    From ! {self(), Md5},
                    loop(Cache);
                {ok, {_OldLastModified, _OldMd5}} ->
                    print("re-cached"),
                    Md5 = file_md5(Filename),
                    NewCache = Cache # { Filename := {LastModified, Md5}},
                    From ! {self(), Md5},
                    loop(NewCache);
                error ->
                    print("cached in"),
                    Md5 = file_md5(Filename),
                    From ! {self(), Md5},
                    NewCache = Cache # { Filename => {LastModified, Md5}},
                    loop(NewCache)
            end;
        stop -> exit(normal);
        Any ->
            error_logger:error_msg("Received:~p~ncache:~p~n", [Any]),
            loop(Cache)
    end.

print(Message) ->
    io:format("~p~n", [Message]).

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
