-module(ejson_error).

-include_lib("eunit/include/eunit.hrl").

-json({series, {list, metric}}).
-json({metric, {string, name}, value}).

no_record_test_() ->
    ?_assertEqual({error, {no_such_record, person}},
                  ejson:to_json_modules({person, "Joe"}, [?MODULE])).

proplist_test_() ->
    Rec = {rec, [{p1, 10}, {p2, false}, {p3, 4.5}]},
    {ok, Enc} = ejson_encode:encode(Rec, [{rec, {proplist, "test"}}]),
    {_, Props} = lists:keyfind(<<"test">>, 1, Enc),
    [?_assert(lists:member({<<"p1">>, 10}, Props)),
     ?_assert(lists:member({<<"p2">>, false}, Props)),
     ?_assert(lists:member({<<"p3">>, 4.5}, Props))].

duplicate_record_test_() ->
    ?_assertEqual({error, {duplicate_records, [a]}},
                  ejson_encode:encode(1, [{a, b}, {a, {atom, c}}, {a, d}])).

duplicate_field_test_() ->
    F = fun(Opt) ->
            Result = ejson_encode:encode(1, Opt),
            io:format("~p~n", [Result]),
            {error, {duplicate_fields, [a]}} =:= Result
        end,
    [
     ?_assert(F([{rec, a, a}])),
     ?_assert(F([{rec, {atom, a}, a}])),
     ?_assert(F([{rec, {string, a}, {binary, a}}])),
     ?_assert(F([{rec, a, {rec_fun, a, f}}])),
     ?_assert(F([{rec, a, {field_fun, a, ff, ff2}}])),
     ?_assert(F([{rec, {const, a, 1}, {list, a}}]))
    ].
