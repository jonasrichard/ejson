-module(ejson_default).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

%% Test for default value for address
-json({book, {string, "title"}, {list, "authors"}, "year"}).
%% People can be authors
-json({person, {string, "name"},
               {string, "address", [{default, "No address"}]}}).

default_test_() ->
    B = {book, "Treasure Island", [{person, "Robert Louis Stevenson"}], 1911},
    Enc = to_json(B),
    Dec = from_json(Enc),
    ?debugVal(Dec).
