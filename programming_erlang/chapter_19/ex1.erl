-module(ex1).
-export([ index_functions/0, lookup/2, test/0]).

index_functions() ->
    Table = ets:new(?MODULE,  [bag, named_table]),
    Modules = [Module || {Module, _BeamFile} <- code:all_loaded()],
    ets:insert(Table,
               [{{Function, Arity}, Module} ||
                Module <- Modules,
                {Function, Arity} <- Module:module_info(exports)]).

lookup(Function, Arity) ->
    ets:lookup(?MODULE, {Function, Arity}).

test() ->
    index_functions(),
    lookup(open, 1).
