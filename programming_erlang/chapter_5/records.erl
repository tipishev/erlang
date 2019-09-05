-module(records).
-export([clear_status/1]).
-record(todo, {status=reminder, who=joe, text}).

clear_status(#todo{status=_S, who=_W} = R) ->
    R#todo{status=finished}.
