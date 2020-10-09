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

gather_urls("<a href" ++ Tail, List) ->
    {Url, Tail1} = collect_url_body(Tail, reverse("<a href")),
    gather_urls(Tail1, [Url|List]);
gather_urls([_|Tail], List) ->
    gather_urls(Tail, List);
gather_urls([], List) -> List.

collect_url_body("</a>" ++ Tail, List) -> {reverse(List, "</a>"), Tail};
collect_url_body([Head|Tail], List) -> collect_url_body(Tail, [Head|List]);
collect_url_body([], _) -> {[], []}.
