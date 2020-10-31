-module(count).

-export([ init/0, me/2, poll/0, test/0]).

init() ->
    ets:new(?MODULE, [set, named_table]).

% TODO use ets:update_counter
me(Module, LineNumber) ->
    Key = {Module, LineNumber},
    case ets:lookup(?MODULE, Key) of
        [] ->
            ets:insert(?MODULE, {Key, 1});
        [{Key, Count}] ->
            ets:insert(?MODULE, {Key, Count + 1})
    end.

poll() ->
    format_table(?MODULE).

format_table(Table) ->
    format_table(Table, ets:first(?MODULE)).

format_table(Table, KeyOrEndOfTable) ->
    case KeyOrEndOfTable  of
        '$end_of_table' -> io:format("----------~n");
        {Module, LineNumber} = Key ->
            [{Key, Count}] = ets:lookup(Table, Key),
            io:format("~p\t~p\t~p~n", [Module, LineNumber, Count]),
            format_table(Table, ets:next(Table, Key))
    end.

test() ->
    _Table = init(),
    me(foo, 42),
    me(bar, 25),
    me(foo, 42),
    me(foo, 42),
    me(?MODULE, ?LINE),
    poll().
