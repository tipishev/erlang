-module(day1).
-export([fuel_required/1]).

fuel_required(Mass) when is_integer(Mass) ->
    Mass div 3 - 2.
