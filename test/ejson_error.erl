-module(ejson_error).

-include_lib("eunit/include/eunit.hrl").

-json({series, {list, metric}}).
-json({metric, {string, name}, {number, value}}).

no_record_test_() ->
    ?_assertEqual({error, {no_such_record, person}},
                  ejson:to_json_modules({person, "Joe"}, [?MODULE])).

duplicate_record_test_() ->
    ?_assertEqual({error, {duplicate_records, [a]}},
                  ejson_encode:encode(1, [{a, b}, {a, {atom, c}}, {a, d}])).

duplicate_field_test_() ->
    F = fun(Opt) ->
            Result = ejson_encode:encode(1, Opt),
            io:format("~p~n", [Result]),
            {error, {duplicate_fields, [<<"a">>]}} =:= Result
        end,
    [
     ?_assert(F([{rec, {string, a}, {binary, a}}])),
     ?_assert(F([{rec, {number, a}, {list, a}}])),
     ?_assert(F([{rec, {boolean, a}, {generic, a, []}}])),
     ?_assert(F([{rec, {const, a, 1}, {list, a}}]))
    ].

error_conflict_test_() ->
    Opts = [{message, {string, error, [{default, "No errors"}]}}],
    Record = {message, "Syntax error"},
    {ok, E} = ejson_encode:encode(Record, Opts),
    {ok, D} = ejson_decode:decode(E, Opts, message),
    ?_assertEqual(Record, D).
