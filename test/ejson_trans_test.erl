-module(ejson_trans_test).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

-json({square, "side"}).

-json({book, {string, "title"}, "author", "year"}).
-json({author, {string, "firstName"},
               {string, "midName", [{default, ""}]},
               {string, "lastName"}}).

simple_test_() ->
    Record = {square, 50},
    {ok, Json} = to_json(Record),
    {ok, Square} = from_json(Json),
    ?_assert(Record =:= Square).

book_test_() ->
    A1 = {author, "John", undefined, "Smith"},
    A2 = {author, "John", "Davison", "Rockefeller"},
    B1 = {book, "The theory of markets", A2, 1933},
    B2 = {book, "How to get things done in 24 hours", A1, 2014},
    {ok, J1} = to_json(B1),
    {ok, J2} = to_json(B2),
    {ok, D1} = from_json(J1),
    {ok, D2} = from_json(J2),
    [?_assertEqual(D1, B1),
     ?_assertEqual(D2, B2)].
