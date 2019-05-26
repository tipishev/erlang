-module(config_reader).
-export([read_config/1]).

read_config(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    Txt = file:read(File, 1024 * 1024),
    % io:fwrite("~p~n", [Txt]).
    jiffy:decode(Txt).

