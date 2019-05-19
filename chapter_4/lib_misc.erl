-module(lib_misc).
-export([permutations/1]).

permutations([]) -> [[]];
permutations(L) -> [[H|T] || H <- L, T <- permutations(L--[H])].
