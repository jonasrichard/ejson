-module(ejson_import_test).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

-json_include([ejson_trans_test]).

-err({testing}).

-json({library, {string, name}, {list, books, [{type, book}]}}).

include_test_() ->
    A1 = {author, "John", undefined, "Smith"},
    A2 = {author, "John", "Davison", "Rockefeller"},
    L = {library, "home",
         [{book, "How to draw horses", A1, 1987},
          {book, "History of rats", A2, 1944}]},
    {ok, E} = to_json(L),
    {ok, D} = from_json(E, library),
    {"Include enc/dec should not fail", [?_assertEqual(L, D)]}.
