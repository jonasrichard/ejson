-module(ejson_error).

-include_lib("eunit/include/eunit.hrl").

-json({series, {list, metric}}).
-json({metric, {string, name}, {number, value}}).

no_record_test_() ->
    {"No such record as person",
     ?_assertEqual({error, {no_such_record, person}},
                   ejson:to_json_modules({person, "Joe"}, [?MODULE]))}.

duplicate_record_test_() ->
    {"Duplicate record error",
     ?_assertEqual({error, {duplicate_records, [a]}},
                   ejson_encode:encode(1, [{a, b}, {a, {atom, c}}, {a, d}], []))}.

duplicate_field_test_() ->
    F = fun(Opt) ->
            Result = ejson_encode:encode(1, Opt, []),
            io:format("~p~n", [Result]),
            {error, {duplicate_fields, [<<"a">>]}} =:= Result
        end,
    {"Duplicate typed field test",
     [
      ?_assert(F([{rec, {string, a}, {binary, a}}])),
      ?_assert(F([{rec, {number, a}, {list, a}}])),
      ?_assert(F([{rec, {boolean, a}, {generic, a, []}}])),
      ?_assert(F([{rec, {const, a, 1}, {list, a}}]))
     ]
    }.

error_conflict_test_() ->
    Rules = [{message, {string, error, [{default, "No errors"}]}}],
    Record = {message, "Syntax error"},
    {ok, E} = ejson_encode:encode(Record, Rules, []),
    {ok, D} = ejson_decode:decode(E, message, Rules, []),
    {"'error' is a valid field name", ?_assertEqual(Record, D)}.

embedded_record_field_error_test_() ->
    Rules = [{book, {string, "title"}, {record, author}},
             {author, {string, "name"}}],
    
    {"Type error in embedded record",
     [?_assertMatch(
         {error, {string_value_expected, "name", _}},
         ejson_encode:encode({book, "The", {author, 12}}, Rules, [])
        )
     ]
    }.
