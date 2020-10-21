-module(exercises).
-export([ex1/0]).

% ex1() -> get_url("bash.org").
% ex1() -> get_url("slashdot.org").
ex1() -> get_url("ya.ru").
% ex1() -> get_url("google.com").  % weird HTTP

%%% Underhood


get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    BinResponse = receive_data(Socket, _ReceivedSoFar=[]),
    parse_response(BinResponse).

-spec parse_response(BinResponse) ->
	Response when
	  BinResponse :: binary(),
	  Response :: {Status :: atom(), Content :: string()}.

%% retun headers, status, body, etc. 
parse_response(BinResponse) ->
    StringResponse = binary_to_list(BinResponse),
    Lines = string:tokens(StringResponse, "\r\n"),
    [Header|_Rest] = Lines,
    parse_header(Header).

-spec parse_header(Header:: string()) ->
    HeaderInfo :: {ok, {Code :: non_neg_integer(),
			Message :: nonempty_string()} 
		   | error, Error :: {unknown_protocol, nonempty_string()}}.

parse_header(Header) ->
    [Protocol | [Code | Message]] = string:tokens(Header, " "),
    case lists:member(Protocol, ["HTTP/1.1", "HTTP/1.0"]) of
        true ->
            {ok, {list_to_integer(Code), Message}};  % FIXME join the status message
        false -> 
            {error, {unknown_protocol, Protocol}}
    end.


receive_data(Socket, ReceivedSoFar) ->
    receive
        {tcp, Socket, BinFragment} ->
            receive_data(Socket, [BinFragment|ReceivedSoFar]);
        {tcp_closed, Socket} ->
            % [second, first] -> [first, second] for efficiency
            TotalResponse = lists:reverse(ReceivedSoFar),
            list_to_binary(TotalResponse)
    end.
