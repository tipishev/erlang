-module(day2).

-behaviour(aoc_solution).

%% aoc_solution callbacks
-export([solve_part1/1, solve_part2/1]).

%% for tests
-export([compute/1]).

%%% solution behavior

solve_part1(_Input) ->
    undefined.

solve_part2(_Input) ->
    undefined.

%%% internals

compute(Input) ->
    Operations = array:fix(array:from_list(Input)),
    compute(Operations, _Current=0).

compute(Operations, Current) ->
    OpCode = array:get(Current, Operations),
    case OpCode of
        1 ->  % add
            A_pos = array:get(Current + 1, Operations),
            B_pos = array:get(Current + 2, Operations),
            Out_pos = array:get(Current + 3, Operations),
            A = array:get(A_pos, Operations),
            B = array:get(B_pos, Operations),
            NewOperations = array:set(Out_pos, A + B, Operations),
            compute(NewOperations, Current + 4);
        2 ->  % multiply
            A_pos = array:get(Current + 1, Operations),
            B_pos = array:get(Current + 2, Operations),
            Out_pos = array:get(Current + 3, Operations),
            A = array:get(A_pos, Operations),
            B = array:get(B_pos, Operations),
            NewOperations = array:set(Out_pos, A * B, Operations),
            compute(NewOperations, Current + 4);
        99 -> array:to_list(Operations);
        _ -> throw({bad_opcode, OpCode})
    end.
