-module(try_test).
-export([gen_exc/1]).

gen_exc(1) -> a;
gen_exc(2) -> throw(a).
