-module(math_functions).
-export([odd/1, even/1, filter/2]).

odd(Number) -> Number rem 2 =:= 1.  
even(Number) -> Number rem 2 =:= 0.  




