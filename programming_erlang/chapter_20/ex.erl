-module(ex).

-export([init_db/0]).

% ex1
-record(user, {name, email, password}).
-record(tip, {url, description, review_date}).
-record(abuse, {name, ip, tips_today}).

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users, [{attributes, record_info(fields, user)}]),
    mnesia:create_table(tips, [{attributes, record_info(fields, tip)}]),
    mnesia:create_table(abuses, [{attributes, record_info(fields, abuse)}]),
    mnesia:stop().
