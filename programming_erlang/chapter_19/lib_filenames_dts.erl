-module(lib_filenames_dts).
-export([
    open/1,
    close/0
    % test/0,
    % filename2index/1,
    % index2filename/1,
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
