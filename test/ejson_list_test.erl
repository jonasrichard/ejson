-module(ejson_list_test).

-import(ejson_test_util, [json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

list_test() ->
    Options = [{book, {string, "title"}, "numberOfPages"},
               {person, {string, "name"}, {list, "books"}}],
    
    Book1 = {book, "Introduction to clean coding", 251},
    Book2 = {book, "TDD - the easy way", 760},
    Person = {person, "Sam", [Book1, Book2]},

    J = ejson_encode:encode(Person, Options),

    ?assertEqual(<<"Sam">>, json_prop(J, "name")),

    Bs = json_prop(J, "books"),
    [B1, B2 ] = Bs,

    ?assertEqual(2, length(Bs)),
    ?assertEqual(<<"Introduction to clean coding">>, json_prop(B1, "title")),
    ?assertEqual(251, json_prop(B1, "numberOfPages")),
    ?assertEqual(<<"TDD - the easy way">>, json_prop(B2, "title")),
    ?assertEqual(760, json_prop(B2, "numberOfPages")).

proplist_test() ->
    Square = {shape, square, [{a, 10}]},
    Circle = {shape, circle, [{radius, 5}, filled]},
    Rect = {shape, rect, [{x_left, 10}, {y_left, 15},
                          {x_right, 50}, {y_right, 30}]},

    Options = [{shape, {atom, "type"}, {proplist, "data"}},
               {shapes, {list, "shapes"}}],
    
    Shapes = ejson_encode:encode({shapes, [Square, Circle, Rect]}, Options),
    
    Ss = json_prop(Shapes, "shapes"),
    ?assertEqual(3, length(Ss)),
    
    [S1, S2, S3] = Ss,
    
    ?assertEqual(<<"square">>, json_prop(S1, "type")),
    S1d = json_prop(S1, "data"),
    ?assertEqual(10, json_prop(S1d, "a")),

    ?assertEqual(<<"circle">>, json_prop(S2, "type")),
    S2d = json_prop(S2, "data"),
    ?assertEqual(5, json_prop(S2d, "radius")),

    ?assertEqual(<<"rect">>, json_prop(S3, "type")),
    S3d = json_prop(S3, "data"),
    ?assertEqual(10, json_prop(S3d, "xLeft")),
    ?assertEqual(15, json_prop(S3d, "yLeft")),
    ?assertEqual(50, json_prop(S3d, "xRight")),
    ?assertEqual(30, json_prop(S3d, "yRight")).

%%list_error_test() ->
%%    Opts = [{book, ["title", {list, "authors"}]}],
%%
%%    ?assertThrow({error, {not_a_list, _}},
%%                 ejson:to_json({book, "Title", 12}, Opts)).
%%
%%proplist_error_test() ->
%%    Opts = [{canvas, [{proplist, "props"}]}],
%%
%%    ?assertThrow({error, {not_a_proplist, _}},
%%                 ejson:to_json({canvas, 12}, Opts)).
%%
%%many_prop_test() ->
%%    Opts = [{canvas, [{proplist, "props"}]}],
%%
%%    J = ejson:to_json({canvas, [{a, 2}, {a, 3}]}, Opts),
%%    ?assertEqual([2, 3], json_prop(json_prop(J, "props"), "a")).
