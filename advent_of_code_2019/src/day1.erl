-module(day1).

%% for parent module
-export([solve_part1/1]).

%% for tests
-export([fuel_required/1]).

fuel_required(Mass) when is_integer(Mass) ->
    Mass div 3 - 2.

solve_part1(ModuleMasses) ->
    FuelRequirements = lists:map(fun day1:fuel_required/1, ModuleMasses),
    lists:sum(FuelRequirements).
