-module(ejson_typed_test).

-include_lib("eunit/include/eunit.hrl").

specified_rec_type_test() ->
    Rules = [{book, {string, title}, {list, "authors", [{type, author}]}},
             {author, {string, name}}],
    Rec = {book, "History of Rome", [{author, "John Smith"},
                                     {author, "Bob Doe"}]},
    {ok, J} = ejson:to_jsx(Rec, Rules, []),
    
    % Remove __rec type information
    J2 = deep_delete_rec(J),
    
    {ok, R2} = ejson_decode:decode(J2, book, Rules, []),
    {book, "History of Rome", Authors} = R2,
    {author, "John Smith"} = hd(Authors).

deep_delete_rec([]) ->
    [];
deep_delete_rec([H | T]) when is_list(H) ->
    [deep_delete_rec(H)] ++ deep_delete_rec(T);
deep_delete_rec([{<<"__rec">>, _} | T]) ->
    deep_delete_rec(T);
deep_delete_rec([{A, V} | T]) when is_list(V) ->
    [{A, deep_delete_rec(V)} | deep_delete_rec(T)];
deep_delete_rec([H | T]) ->
    [H | deep_delete_rec(T)].

root_record_rec_test_() ->
    Rules = [{book, {string, title}, {list, "authors", [{type, author}]}},
             {author, {string, name}}],
    Opts = [{type_field, [book]}],
    Rec = {book, "History of Rome", [{author, "John Smith"},
                                     {author, "Bob Doe"}]},

    {ok, J} = ejson:to_json(Rec, Rules, [{type_field, [book]}]),
    D = jsx:decode(J),
    {<<"__rec">>, <<"book">>} = lists:keyfind(<<"__rec">>, 1, D),
    {ok, B} = ejson:from_json(J, Rules, Opts),
    [?_assertEqual(Rec, B)].
