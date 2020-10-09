-module(lib_find).

-export([files/3, files/5]).
-import(lists, [reverse/1]).

-include_lib("kernel/include/file.hrl").

files(Dir, ShellRegExp, IsRecursive) ->
    AwkRegExp = xmerl:sh_to_awk(ShellRegExp).
    reverse
    


