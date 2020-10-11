-module(lib_find).

-export([files/3, files/5]).
-import(lists, [reverse/1]).

-include_lib("kernel/include/file.hrl").

files(Dir, ShellRegExp, IsRecursive) ->
    AwkRegExp = xmerl_regexp:sh_to_awk(ShellRegExp),
    reverse(files(Dir, AwkRegExp, IsRecursive,
                 % simply collect the files
                 fun(File, Acc) -> [File|Acc] end, [])).

files(Dir, AwkRegExp, IsRecursive, Fun, Acc) ->
    case file:list_dir(Dir) of
        {ok, Files} -> find_files(Files, Dir, AwkRegExp,
                                  IsRecursive, Fun, Acc);
        {error, _} -> Acc
    end.

find_files([File|Tail], Dir, AwkRegExp, IsRecursive, Fun, Acc) ->
    FullPath = filename:join(Dir, File),
    case filetype(FullPath) of 
        regular ->
            case re:run(FullPath, AwkRegExp, [{capture, none}]) of
                match -> 
                    NewAcc = Fun(FullPath, Acc),
                    find_files(Tail, Dir, AwkRegExp, IsRecursive, Fun, NewAcc);
                nomatch -> 
                    find_files(Tail, Dir, AwkRegExp, IsRecursive, Fun, Acc)
            end;
        directory ->
            case IsRecursive of
                true ->
                    NewAcc = files(FullPath, AwkRegExp, IsRecursive, Fun, Acc),
                    files(Tail, AwkRegExp, IsRecursive, Fun, NewAcc);
                false ->
                    files(Tail, AwkRegExp, IsRecursive, Fun, Acc)
            end;
        error ->
            find_files(Tail, Dir, AwkRegExp, IsRecursive, Fun, Acc)
    end;
find_files([], _, _, _, _, A) -> A.

filetype(FullPath) ->
    case file:read_file_info(FullPath) of
        {ok, Facts} ->
            case Facts#file_info.type of
                regular -> regular;
                directory -> directory;
                _ -> error
            end;
        _ ->
            error
    end.
