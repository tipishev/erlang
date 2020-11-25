-module(day1).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([fuel_required_naive/1, fuel_required_compound/1]).

%%% solution behavior

solve_part1(ModuleMasses) ->
    FuelRequirements = lists:map(fun day1:fuel_required_naive/1, ModuleMasses),
    lists:sum(FuelRequirements).

solve_part2(ModuleMasses) ->
    FuelRequirements = lists:map(fun day1:fuel_required_compound/1, ModuleMasses),
    lists:sum(FuelRequirements).

%%% internals

fuel_required_naive(Mass) when is_integer(Mass) ->
    Mass div 3 - 2.

fuel_required_compound(Mass) when is_integer(Mass) ->
    Naive = fuel_required_naive(Mass),
    fuel_required_compound(Naive, Naive).

fuel_required_compound(FuelMass, Total) ->
    ExtraFuel = fuel_required_naive(FuelMass),  
    case ExtraFuel =< 0 of
        true -> Total;
        false -> fuel_required_compound(ExtraFuel, Total + ExtraFuel)
    end.

