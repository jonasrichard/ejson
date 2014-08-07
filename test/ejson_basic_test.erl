-module(ejson_basic_test).

-include_lib("eunit/include/eunit.hrl").

value_test_() ->
    Cases = [1, 3.4, true, false],
    [?_assert(Expected =:= ejson_encode:encode(Expected, []))
        || Expected <- Cases].

null_test_() ->
    ?_assert(null =:= ejson_encode:encode(undefined, [])).

list_test_() ->
    Cases = [
             [],
             [1, 2],
             [true, 9],
             [6.55, false],
             [1, [], [2, 3]]
            ],
    [?_assert(Expected =:= ejson_encode:encode(Expected, []))
        || Expected <- Cases].

proplist_test_() ->
    Rec = {rec, [{p1, 10}, {p2, false}, {p3, 4.5}]},
    Enc = ejson_encode:encode(Rec, [{rec, {proplist, "test"}}]),
    {_, Props} = lists:keyfind(<<"test">>, 1, Enc),
    [?_assert(lists:member({<<"p1">>, 10}, Props)),
     ?_assert(lists:member({<<"p2">>, false}, Props)),
     ?_assert(lists:member({<<"p3">>, 4.5}, Props))].

duplicate_record_test_() ->
    ?_assert({error, duplicate_record_names} =:=
             ejson_encode:encode(1, [{a, b}, {a, {atom, c}}])).

duplicate_field_test_() ->
    F = fun(Opt) ->
            Result = ejson_encode:encode(1, Opt),
            {error, duplicate_field_name} =:= Result
        end,
    [
     ?_assert(F([{rec, a, a}])),
     ?_assert(F([{rec, {atom, a}, a}])),
     ?_assert(F([{rec, {string, a}, {binary, a}}])),
     ?_assert(F([{rec, a, {rec_fun, a, f}}])),
     ?_assert(F([{rec, a, {field_fun, a, ff, ff2}}])),
     ?_assert(F([{rec, {const, a, 1}, {list, a}}]))
    ].

field_fun_test_() ->
    Enc = fun(Num) -> Num * 1000 end,
    Dec = fun(Num) -> Num div 1000 end,
    Opts = [{time, {field_fun, "jsTime", Enc, Dec}}],
    Record = {time, 2300},
    E = ejson_encode:encode(Record, Opts),
    D = ejson_decode:decode(E, Opts),
    ?_assert(Record =:= D).
