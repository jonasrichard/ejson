-module(ejson_list_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

list_test() ->
    Options = [{book, ["title", "numberOfPages"]},
               {person, ["name", {list, "books"}]}],
    
    Book1 = {book, "Introduction to clean coding", 251},
    Book2 = {book, "TDD - the easy way", 760},
    Person = {person, "Sam", [Book1, Book2]},

    J = ejson:conv(Person, Options),

    ?assertEqual(<<"Sam">>, ejson:json_prop(J, "name")),

    Bs = ejson:json_prop(J, "books"),
    [B1, B2 ] = Bs,

    ?assertEqual(2, length(Bs)),
    ?assertEqual(<<"Introduction to clean coding">>,
                 ejson:json_prop(B1, "title")),
    ?assertEqual(251, ejson:json_prop(B1, "numberOfPages")),
    ?assertEqual(<<"TDD - the easy way">>, ejson:json_prop(B2, "title")),
    ?assertEqual(760, ejson:json_prop(B2, "numberOfPages")).

proplist_test() ->
    Square = {shape, square, [{a, 10}]},
    Circle = {shape, circle, [{radius, 5}]},
    Rect = {shape, rect, [{x_left, 10}, {y_left, 15},
                          {x_right, 50}, {y_right, 30}]},

    Options = [{shape, ["type", {proplist, "data"}]}].

-endif.
