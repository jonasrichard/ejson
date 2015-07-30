-module(ejson_default).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

%% Test for default value for address
-json({book, {string, "title"}, {list, "authors"}, {number, "year"}}).
%% People can be authors
-json({person, {string, "name"},
               {string, "address", [{default, "No address"}]}}).

default_test() ->
    B = {book, "Treasure Island", [{person, "Robert Louis Stevenson"}], 1911},
    {ok, Enc} = to_json(B),
    % ?debugVal(Enc),
    {ok, Dec} = from_json(Enc),
    % ?debugVal(Dec),
    {book, _, [{person, _, undefined}], 1911} = Dec.
