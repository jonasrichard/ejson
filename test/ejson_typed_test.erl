-module(ejson_typed_test).

-include_lib("eunit/include/eunit.hrl").

-import(ejson_test_util, [jsx_path/2, json_path/2]).

specified_rec_type_test_() ->
    Rules = [{book, {string, title}, {list, "authors", [{type, author}]}},
             {author, {string, name}}],
    Rec = {book, "History of Rome", [{author, "John Smith"},
                                     {author, "Bob Doe"}]},
    
    {ok, J} = ejson:to_jsx(Rec, Rules, []),
    {ok, R2} = ejson_decode:decode(J, book, Rules, []),

    [{"No __rec fields in the list",
      ?_assertEqual(undefined, jsx_path(J, "authors.1.__rec"))},
     {"Typed list is decoded",
      ?_assertEqual({author, "John Smith"}, hd(element(3, R2)))}].

root_record_rec_test_() ->
    Rules = [{book, {string, title}, {list, "authors", [{type, author}]}},
             {author, {string, name}}],
    Opts = [{type_field, [book]}],
    Rec = {book, "History of Rome", [{author, "John Smith"},
                                     {author, "Bob Doe"}]},

    {ok, J} = ejson:to_json(Rec, Rules, [{type_field, [book]}]),
    {ok, R} = ejson:from_json(J, Rules, Opts),
    
    [{"__rec field generated for specific root records",
      ?_assertEqual(<<"book">>, json_path(J, "__rec"))},
     {"Decode works since __rec defined",
      ?_assertEqual(Rec, R)}].

record_type_mismatch_test_() ->
    Rules = [{book, {string, title},
                    {record, author, [{type, author}]}},
             {author, {string, name}}],
    Rec = {book, "Comprehensive guide to Javascript",
                 {author2, "John Mismatch"}},
    Mismatch = ejson:to_json(Rec, Rules, []),
    [?_assertEqual({error, {mismatched_record, author2, author}}, Mismatch)].
