-module(scavenge_urls).
-export([urls2htmlFile/2, bin2urls/1]).
-import(lists, [reverse/1, reverse/2, map/2]).

% writes I/O list to file all at once
urls2htmlFile(Urls, Filename) ->
    file:write_file(Filename, urls2html(Urls)).

bin2urls(Bin) ->
    HtmlString = binary_to_list(Bin),
    gather_urls(HtmlString, _Accumulator=[]).

% here we start making the HTML IO-list
urls2html(Urls) -> [h1("Urls"), make_html_list(Urls)].

h1(Title) -> ["<h1>", Title, "</h1>\n"].

make_html_list(List) ->
    ["<ul>\n",
     % no attempt to flatten
     map(fun(Element) -> ["<li>", Element, "</li>\n"] end, List),
     "</ul>\n"].

gather_urls("<a href" ++ T, L) ->
    {Url, T1} = collect_url_body(T, reverse("<a href")),
    gather_urls(T1, [Url|L]);
gather_urls([_|T], L) ->
    gather_urls(T, L);
gather_urls([], L) -> L.

collect_url_body("</a>" ++ T, L) -> {reverse(L, "</a>"), T};
collect_url_body([H|T], L) -> collect_url_body(T, [H|L]);
collect_url_body([], _) -> {[], []}.
