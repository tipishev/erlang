-module(lib_filenames_dts).
-export([
    open/1,
    close/0,
    % test/0,
    filename2index/1,
    index2filename/1
]).

open(Filename) ->
    io:format("dets opened:~p~n", [Filename]),
    IsFile = filelib:is_file(Filename),
    case dets:open_file(?MODULE, [{file, Filename}]) of
        {ok, ?MODULE} ->
            case IsFile of
                true -> void;
                false -> ok = dets:insert(?MODULE, {free, 1})
            end,
            true;
        {error, Reason} ->
            io:format("cannot open dets table~n"),
            exit({eDetsOpen, Reason})
    end.

close() -> dets:close(?MODULE).

filename2index(Filename) when is_binary(Filename) ->
    case dets:lookup(?MODULE, Filename) of
        [] ->
            [{_, Free}] = dets:lookup(?MODULE, free),
            ok = dets:insert(
                   ?MODULE,
                   [{Free, Filename},
                    {Filename, Free},
                    {free, Free + 1}]),
            Free;
        [{_, N}] ->
            N
    end.

index2filename(Index) when is_integer(Index) ->
    case dets:lookup(?MODULE, Index) of
        [] -> error;
        [{_Index, Filename}] -> Filename
    end.
