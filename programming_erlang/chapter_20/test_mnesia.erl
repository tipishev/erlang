-module(test_mnesia).
-export([do_this_once/0]).

% TODO prefix record names to avoid clashes with other apps
-record(shop, {item, quantity, cost}).
-record(cost, {name, price}).

do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(shop, [{attributes, record_info(fields, shop)}]),
    mnesia:create_table(cost, [{attributes, record_info(fields, cost)}]),
    mnesia:stop().


