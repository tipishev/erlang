-module(ex).

-export([init_db/0, add_user/3]).

%% development functions
-export([test/0, reset_tables/0, start/0]).


% ex1
-record(user, {name, email, password}).
-record(tip, {url, description, review_date}).
-record(abuse, {name, ip, date, tips_count}).

test() ->
    add_user("tipishev", "tipishev@gmail.com", "hunter2").

init_db() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(users, [{attributes, record_info(fields, user)}]),
    mnesia:create_table(tips, [{attributes, record_info(fields, tip)}]),
    mnesia:create_table(abuses, [{attributes, record_info(fields, abuse)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([users, tips, abuses], 20000).

example_tables() ->
    [
     {user, "tipishev", "tipishev@gmail.com", "hunter2"},
     {user, "alice", "alice@example.com", "EveIknowUAreReadingThis!"},

     {tip, "erlang.org", "all you need to know.", {2020, 11, 04}},

     {abuse, "tipishev", {127, 0, 0, 1}, {2020, 11, 04}, 1}
    ].

reset_tables() ->
    mnesia:clear_table(users),
    mnesia:clear_table(tips),
    mnesia:clear_table(abuses),
    F = fun() ->
		lists:foreach(fun mnesia:write/1, example_tables())
	end,
    mnesia:transaction(F).


do(Query) ->
    F = fun() -> qlc:e(Query) end,
    {atomic, Value} = mnesia:transaction(F),
    Value.

add_user(Name, Email, Password) ->
    % TODO Use a salted password hash.
    User = #user{name=Name, email=Email, password=Password},
    F = fun() -> mnesia:write(User) end,
    mnesia:transaction(F).
                 

