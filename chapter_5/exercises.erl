-module(exercises).
-export([map_search_pred/2]).

map_search_pred(Map, Pred) ->
    helper(maps:to_list(Map), Pred).

helper([{Key, Value}|T], Pred) ->
    case Pred(Key, Value) of
        true -> {Key, Value};
        false -> helper(T, Pred) 
    end;
helper([], _Pred) -> nope.

% Map = #{1 => 2, 3 => 4, 6 => 8, 8 => 13}.
% Pred = fun(Key, Value) -> Key + Value =:= 14 end.
% c(exercises).
% exercises:map_search_pred(Map, Pred).

