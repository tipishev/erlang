-module(day4).
-behaviour(aoc_solution).

%%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%%% test exports
-export([check/1, check_groups/1]).

%%% solution behavior

solve_part1({PossibleMin, PossibleMax}) ->
    Passwordy = [Password || Password <- lists:seq(PossibleMin, PossibleMax),
                             check(Password)],
    length(Passwordy).
solve_part2({PossibleMin, PossibleMax}) ->
    Passwordy = [Password || Password <- lists:seq(PossibleMin, PossibleMax), check(Password)],
    length([Password || Password <- Passwordy, check_groups(Password)]).

%%% Part 1
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


%%% Part 2

check_groups(PasswordInt) ->
    [FirstDigit|Rest] = integer_to_list(PasswordInt),
    {_, Counts} = lists:foldl(fun groups/2, {_Curr=FirstDigit, _Counts=[1]}, Rest),
    lists:member(2, Counts).

groups(Curr, {Curr, [Count|Counts]}) ->
    {Curr, [Count + 1 | Counts]};
groups(New, {_Curr, Counts}) ->
    {New, [1 | Counts]}.

    
