-module(ejson_default).

-compile({parse_transform, ejson_trans}).

-import(ejson_test_util, [json_path/2]).

-include_lib("eunit/include/eunit.hrl").

%% Test for default value for address
-json({book, {string, "title"}, {list, "authors", [{type, person}]}, {number, "year"}}).
%% People can be authors
-json({person, {string, "name"},
               {string, "address", [{default, "No address"}]}}).

-json({planet,
       {binary, "name"},
       {number, "population", [{default, undefined}]}}).

default_test_() ->
    B = {book, "Treasure Island", [{person, "Robert Louis Stevenson"}], 1911},
    {ok, Enc} = to_json(B),
    {ok, Dec} = from_json(Enc, book),
    ?_assertMatch({book, _, [{person, _, undefined}], 1911}, Dec).

default_real_test_() ->
    Enc = <<"{\"title\":\"Treasure Island\","
            " \"authors\":[{\"name\":\"Robert Louis Stevenson\"}],"
            " \"year\":1911}">>,
    {ok, Dec} = from_json(Enc, book),
    ?_assertMatch({book, "Treasure Island", [{person, "Robert Louis Stevenson", "No address"}], 1911},
                  Dec).

default_field_test_() ->
%    dbg:tracer(), dbg:tpl(ejson_encode, []), dbg:p(all, c),
    B = {book, "Treasure Island", [{person, "Robert Louis Stevenson"}], 1911},
    {ok, J} = ejson:to_jsx_modules(B, [?MODULE]),
    ?debugVal(J),
    [?_assertEqual(<<"Treasure Island">>, json_path(J, "title")),
     ?_assertEqual(<<"Robert Louis Stevenson">>, json_path(J, "authors.1.name")),
     ?_assertEqual(null, json_path(J, "authors.1.address"))].

default_undefined_test_() ->
    Enc = <<"{\"name\":\"Mars\"}">>,
    {ok, Dec} = from_json(Enc, planet),
    ?_assertMatch({planet, <<"Mars">>, undefined}, Dec).

default_undefined_encode_test_() ->
    P = {planet, <<"Mars">>, undefined},
    {ok, Enc} = to_json(P),
    ?_assertMatch(<<"{\"name\":\"Mars\"}">>, Enc).
