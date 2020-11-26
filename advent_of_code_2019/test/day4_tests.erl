-module(day4_tests).
-include_lib("eunit/include/eunit.hrl").

%%% Tasks

part1_test_() ->
    [

     {"111111 meets these criteria (double 11, never decreases).",
      ?_assert(day4:check(111111))},

     {"223450 does not meet these criteria (decreasing pair of digits 50).",
      ?_assertNot(day4:check(223450))},

     {"123789 does not meet these criteria (no double).",
      ?_assertNot(day4:check(123789))}
    ].


part2_test_() ->
    [

     {"112233 meets these criteria because the digits never
        decrease and all repeated digits are exactly two digits long.",
      ?_assert(day4:check_groups(112233))},

     {"123444 no longer meets the criteria (the repeated 44 is part
        of a larger group of 444).",
      ?_assertNot(day4:check_groups(123444))},

     {"111122 meets the criteria (even though 1 is repeated more
        than twice, it still contains a double 22).",
      ?_assert(day4:check_groups(111122))}

    ].
