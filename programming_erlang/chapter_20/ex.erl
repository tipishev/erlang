-module(ex).

% ex1
-record(user, {name, email, password}).
-record(tip, {url, description, review_date}).
-record(abuse, user, ip, tips_today).
