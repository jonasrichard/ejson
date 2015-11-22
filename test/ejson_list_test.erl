-module(ejson_list_test).

-import(ejson_test_util, [json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

list_test() ->
    Options = [{book, {string, "title"}, {number, "numberOfPages"}},
               {person, {string, "name"}, {list, "books"}}],
    
    Book1 = {book, "Introduction to clean coding", 251},
    Book2 = {book, "TDD - the easy way", 760},
    Person = {person, "Sam", [Book1, Book2]},

    {ok, J} = ejson_encode:encode(Person, Options),

    ?assertEqual(<<"Sam">>, json_prop(J, "name")),

    Bs = json_prop(J, "books"),
    [B1, B2 ] = Bs,

    ?assertEqual(2, length(Bs)),
    ?assertEqual(<<"Introduction to clean coding">>, json_prop(B1, "title")),
    ?assertEqual(251, json_prop(B1, "numberOfPages")),
    ?assertEqual(<<"TDD - the easy way">>, json_prop(B2, "title")),
    ?assertEqual(760, json_prop(B2, "numberOfPages")).

mixed_list_test() ->
    Options = [{shapes, {list, shapes}},
               {square, {number, a}},
               {rect, {number, a}, {number, b}},
               {circle, {number, r}}],

    Shapes = {shapes, [{square, 1},
                       {square, 3},
                       {rect, 2, 3},
                       {circle, 5}]},

    {ok, J} = ejson_encode:encode(Shapes, Options),

    ShapeList = json_prop(J, "shapes"),
    ?assertEqual(4, length(ShapeList)),

    [Sq1, Sq2, R1, C1] = ShapeList,
    ?assertEqual(1, json_prop(Sq1, "a")),
    ?assertEqual(3, json_prop(Sq2, "a")),
    ?assertEqual(2, json_prop(R1, "a")),
    ?assertEqual(3, json_prop(R1, "b")),
    ?assertEqual(5, json_prop(C1, "r")),

    {ok, D} = ejson_decode:decode(J, Options, shapes),
    ?assertEqual(Shapes, D).
