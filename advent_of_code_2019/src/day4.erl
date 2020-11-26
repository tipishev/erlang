-module(day4).
-behaviour(aoc_solution).

%%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%%% test exports
-export([check/1]).

%%% solution behavior

solve_part1({PossibleMin, PossibleMax}) ->
    Passwordy = [Password || Password <- lists:seq(PossibleMin, PossibleMax),
                             check(Password)],
    length(Passwordy).
solve_part2({PossibleMin, PossibleMax}) ->
    Passwordy = [Password || Password <- lists:seq(PossibleMin, PossibleMax), check(Password)],
    Passwordy.


check(Password) ->
    Digits = integer_to_list(Password),
    no_double(-1, Digits).

% no_double state
no_double(_, []) -> false;
no_double(OldNumber, [NewDigit | RemainingDigits]) ->
    NewNumber = from_ascii(NewDigit),
    if
        NewNumber > OldNumber ->
            no_double(NewNumber, RemainingDigits);
        NewNumber < OldNumber ->
            false;
        NewNumber =:= OldNumber ->
            double(NewNumber, RemainingDigits)
    end.

% double state
double(_, []) -> true;
double(OldNumber, [NewDigit | RemainingDigits]) ->
    NewNumber = from_ascii(NewDigit),
    if
        NewNumber >= OldNumber ->
            double(NewNumber, RemainingDigits);
        NewNumber < OldNumber ->
            false
    end.

from_ascii(ZeroToNineAscii) ->
    ZeroToNineAscii - 48.
