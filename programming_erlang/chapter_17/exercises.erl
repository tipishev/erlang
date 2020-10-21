-module(exercises).
-export([ex1/0]).

ex1() -> get_url("bash.org"). % 200
% ex1() -> get_url("slashdot.org"). % 301
% ex1() -> get_url("ya.ru").  % 406
% ex1() -> get_url("google.com").  % 200, HTTP/1.0

%%% Underhood


get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    BinResponse = receive_data(Socket, _ReceivedSoFar=[]),
    parse_response(BinResponse).

-spec parse_response(BinResponse :: binary()) ->
    Response :: {
       StatusTag :: atom(),
       body,
       Body :: string(),
       headers,
       #{HeaderKey :: string() => HeaderValue :: string()}
    }.

%% retun headers, status, body, etc. 
parse_response(BinResponse) ->
    StringResponse = binary_to_list(BinResponse),
    [HttpHeader, HeadersAndBody] = string:split(StringResponse, "\r\n"),
    [HeadersString, Body] = string:split(HeadersAndBody, "\r\n\r\n"),
    Headers = parse_headers(HeadersString),
    {ok, {StatusCode, _Message}} = parse_http_header(HttpHeader),
    {status_code_to_human(StatusCode), body, Body, headers, Headers}.

%% parse "{HTTP/1.X} {Code} {Message}"
-spec parse_http_header(HttpHeader:: string()) -> {ok, {Code :: non_neg_integer(), Mesage :: string()}}
                                                  | {error, {unknown_protocol, Protocol :: string()}}.

parse_http_header(HttpHeader) ->
    [Protocol, Rest] = string:split(HttpHeader, " "),
    [Code, Message] = string:split(Rest, " "),
    % {Code, Message} = string:take(Rest, " ", true),  % exercise, leading " "
    case lists:member(Protocol,
                      ["HTTP/1.1", "HTTP/1.0"]) of
        true ->
            {ok, {list_to_integer(Code),
                  % % lists:join(" ", Message)
                  Message
                  % string:prefix(Header, Protocol)
                 }};
        false -> 
            {error, {unknown_protocol, Protocol}}
    end.

%% parse headers string to a map
-spec parse_headers(HeadersString :: string()) ->
    #{HederKey :: string() => HeaderValue :: string()}.

parse_headers(HeadersString) ->
    maps:from_list(
      lists:map(
        fun(KeyColonVal) ->
            list_to_tuple(string:split(KeyColonVal, ": "))
        end,
        string:lexemes(HeadersString, ["\r\n"]))).

%% status code to human
-spec status_code_to_human(StatusCode :: 100..599) -> ok | redirect | you_messed_up | they_messed_up.

status_code_to_human(C) ->
    if
      % TODO a separate function
      C >= 200, C < 300 -> ok;
      C >= 300, C < 400 -> redirect;
      C >= 400, C < 500 -> you_messed_up;
      C >= 500, C < 600 -> they_messed_up
    end.


%% TCP bit
receive_data(Socket, ReceivedSoFar) ->
    receive
        {tcp, Socket, BinFragment} ->
            receive_data(Socket, [BinFragment|ReceivedSoFar]);
        {tcp_closed, Socket} ->
            % [second, first] -> [first, second] for efficiency
            TotalResponse = lists:reverse(ReceivedSoFar),
            list_to_binary(TotalResponse)
    end.
