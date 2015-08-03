-module(ejson_default).

-compile({parse_transform, ejson_trans}).

-import(ejson_test_util, [json_path/2]).

-include_lib("eunit/include/eunit.hrl").

%% Test for default value for address
-json({book, {string, "title"}, {list, "authors"}, {number, "year"}}).
%% People can be authors
-json({person, {string, "name"},
               {string, "address", [{default, "No address"}]}}).

default_test_() ->
    B = {book, "Treasure Island", [{person, "Robert Louis Stevenson"}], 1911},
    {ok, Enc} = to_json(B),
    {ok, Dec} = from_json(Enc, book),
    ?_assertMatch({book, _, [{person, _, undefined}], 1911}, Dec).

default_field_test_() ->
    B = {book, "Treasure Island", [{person, "Robert Louis Stevenson"}], 1911},
    {ok, J} = ejson:to_jsx_modules(B, [?MODULE]),
    ?debugVal(J),
    [?_assertEqual(<<"Treasure Island">>, json_path(J, "title")),
     ?_assertEqual(<<"person">>, json_path(J, "authors.1.__rec")),
     ?_assertEqual(<<"Robert Louis Stevenson">>, json_path(J, "authors.1.name")),
     ?_assertEqual(null, json_path(J, "authors.1.address"))].
