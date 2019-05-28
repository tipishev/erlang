-module(exercises).
-export([read/1]).

read(File) ->
    case file:read_file(File) of
        {ok, Bin} -> Bin;
        {error, enoent} -> throw("File does not exist");
	{error, _} -> throw("Error")
    end.
