-module(exercises).
-export([ex1/0, ex2/0, ex3/0, ex4/0]).

% ex2
-export([start_nano_server/0, nano_client_eval/3]).

ex1() -> get_url("bash.org"). % 200
% ex1() -> get_url("slashdot.org"). % 301
% ex1() -> get_url("ya.ru").  % 406
% ex1() -> get_url("google.com").  % 200, HTTP/1.0

ex2() ->
    spawn(fun() -> start_nano_server() end),
    nano_client_eval(string, reverse, ["!ylranG"]).

ex3() ->
    spawn(fun() -> start_udp_server() end),
    udp_client(lists, reverse, ["Foobar is the best!"]).

ex4() -> ex3().

% ex5() ->
%     email_TCP_server(8008).
    % check_mail(alice).
    % check_mail(bob),
    % send_mail(alice, bob, "Hello!"),
    % check_mail(bob),
    % send_mail(bob, alice, "What's up?"),
    % check_mail(alice),

%%% Underhood

%%% ex4

-spec encrypt(PlainText) -> CypherText when
                            PlainText :: binary(),
                            CypherText :: binary().
encrypt(PlainText) ->
    crypto:start(),
    Key = <<1:128>>,
    IV = <<0:128>>,
    StateEnc = crypto:crypto_init(aes_128_ctr, Key, IV, true),
    CypherText = crypto:crypto_update(StateEnc, PlainText),
    io:format("Encrypted ~p to ~p~n", [PlainText, CypherText]),
    CypherText.
    


-spec decrypt(CypherText) -> PlainText when
                            CypherText :: binary(),
                            PlainText :: binary().
decrypt(CypherText) ->
    crypto:start(),
    Key = <<1:128>>,
    IV = <<0:128>>,
    StateDec = crypto:crypto_init(aes_128_ctr, Key, IV, false),
    PlainText = crypto:crypto_update(StateDec, CypherText),
    io:format("Decrypted ~p to ~p~n", [CypherText, PlainText]),
    PlainText.

%%% ex3, ex4

%% The server
-spec start_udp_server() -> UdpServerPid :: pid().
start_udp_server() ->
    spawn(fun() -> udp_server(4000) end).

-spec udp_server(Port) -> none() when
      Port :: inet:port_number().

udp_server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("UDP server opened socket:~p~n", [Socket]),
    udp_loop(Socket).

-type socket() :: port().
-spec udp_loop(Socket) -> none() when
      Socket :: socket().

udp_loop(Socket) ->
    receive
        {udp, Socket, Host, Port, EncBin} = Msg ->
            io:format("server received: ~p~n", [Msg]),
            Bin = decrypt(EncBin),
            {Ref, {Module, Function, Arguments}} = binary_to_term(Bin),
            FunctionResult =apply(Module, Function, Arguments),
            Output = {Ref, FunctionResult},
            gen_udp:send(Socket, Host, Port, encrypt(term_to_binary(Output))),
            udp_loop(Socket)
    end.

%% The client

-spec udp_client(Module, Function, Args) -> Term | timeout
                            when
      Module :: module(),
      Function :: function(),
      Args :: [term()],
      Term :: term().

udp_client(Module, Function, Args) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client opened socket=~p~n", [Socket]),
    Ref = make_ref(),
    ok = gen_udp:send(Socket, "localhost", 4000,
                      encrypt(term_to_binary({Ref, {Module, Function, Args}}))),
    Response = wait_for_ref(Socket, Ref),
    gen_udp:close(Socket),
    Response.

-spec wait_for_ref(Socket, Ref) -> Term | timeout
                                     when
      Socket :: socket(),
      Ref :: reference(),
      Term :: term().

wait_for_ref(Socket, Ref) ->
receive
       {udp, Socket, _Host, _Port, EncBin} = Msg ->
           io:format("client received:~p~n", [Msg]),
           case binary_to_term(decrypt(EncBin)) of
               {Ref, Response} ->
                   io:format("Yep, that's the one.~n", []),
                   Response;
               {_AnotherRef, _Response} ->
                   io:format("Nope, it's them script-kiddies"
                             " pranking again.~n", []),
                   wait_for_ref(Socket, Ref)
           end
after 2000 ->
         timeout
end.


%%% ex2
start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},
                                         {reuseaddr, true},
                                         {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),  % to avoid receiving more connections
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            MFA = {M, F, A} = binary_to_term(Bin),
            io:format("Server (unpacked) ~p~n", [MFA]),
            Reply = apply(M, F, A),
            io:format("Server replying ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

nano_client_eval(M, F, A) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345,
                                   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary({M, F, A})),
    receive
        {tcp, Socket, Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.



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
