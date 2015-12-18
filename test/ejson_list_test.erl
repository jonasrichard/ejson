-module(ejson_list_test).

-import(ejson_test_util, [json_prop/2, jsx_prop/2, jsx_path/2]).

-include_lib("eunit/include/eunit.hrl").

list_test_() ->
    Options = [{book, {string, "title"}, {number, "numberOfPages"}},
               {person, {string, "name"}, {list, "books"}}],
    
    Book1 = {book, "Introduction to coding", 251},
    Book2 = {book, "TDD - the easy way", 760},
    Person = {person, "Sam", [Book1, Book2]},

    {ok, J} = ejson_encode:encode(Person, Options, []),

    [{"List elements check",
      [
       ?_assertEqual(2, length(jsx_prop(J, "books"))),
       ?_assertEqual(<<"Introduction to coding">>, jsx_path(J, "books.1.title")),
       ?_assertEqual(251, jsx_path(J, "books.1.numberOfPages")),
       ?_assertEqual(<<"TDD - the easy way">>, jsx_path(J, "books.2.title")),
       ?_assertEqual(760, jsx_path(J, "books.2.numberOfPages"))
      ]},
     {"List type check", ?_assertEqual(<<"book">>, jsx_path(J, "books.1.__rec"))}].

mixed_list_test_() ->
    Options = [{shapes, {list, shapes}},
               {square, {number, a}},
               {rect, {number, a}, {number, b}},
               {circle, {number, r}}],

    Shapes = {shapes, [{square, 1},
                       {square, 3},
                       {rect, 2, 3},
                       {circle, 5}]},

    {ok, J} = ejson_encode:encode(Shapes, Options, []),
    {ok, D} = ejson_decode:decode(J, shapes, Options, []),

    [{"Mixed list length",
        ?_assertEqual(4, length(jsx_path(J, "shapes")))},
     {"1st is a square",
        ?_assertEqual(<<"square">>, jsx_path(J, "shapes.1.__rec"))},
     {"4nd is a circle",
        ?_assertEqual(<<"circle">>, jsx_path(J, "shapes.4.__rec"))},
     {"Decoding mixed list",
        ?_assertEqual(Shapes, D)}
    ].

complex_list_test_() ->
    Rules = [{rational, {number, p}, {number, q}},
             {complex, {record, a, [{type, rational}]},
                       {record, b, [{type, rational}]}}],
    Opts = [{type_field, [rational, complex]}],
    List = [5, {rational, 5, 6}, {complex, {rational, 1, 2}, {rational, 5, 3}}],

    {ok, E} = ejson:to_jsx(List, Rules, Opts),
    {ok, D} = ejson:from_json(jsx:encode(E), Rules, Opts),

    [{"3 numbers encoded", ?_assertEqual(3, length(E))},
     {"1st is number", ?_assertEqual(5, jsx_path(E, "1"))},
     {"2nd is rational",
      [?_assertEqual(5, jsx_path(E, "2.p")),
       ?_assertEqual(6, jsx_path(E, "2.q")),
       ?_assertEqual(<<"rational">>, jsx_path(E, "2.__rec"))]},
     {"in the 3rd there is rational",
      ?_assertEqual(<<"rational">>, jsx_path(E, "3.a.__rec"))},
     {"3 numbers decoded", ?_assertEqual(3, length(D))},
     {"2nd is 5/6",
      ?_assertEqual({rational, 5, 6}, lists:nth(2, D))}
    ].
