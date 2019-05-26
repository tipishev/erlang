-module(config_reader).
-export([read_config/1]).

read_config(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    maps:from_json(File).

