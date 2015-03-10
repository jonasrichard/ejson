-module(ejson_basic_test).

-include_lib("eunit/include/eunit.hrl").

value_test_() ->
    Cases = [1, 3.4, true, false],
    [?_assert({ok, Expected} =:= ejson_encode:encode(Expected, []))
        || Expected <- Cases].

null_test_() ->
    ?_assert({ok, null} =:= ejson_encode:encode(undefined, [])).

list_test_() ->
    Cases = [
             [],
             [1, 2],
             [true, 9],
             [6.55, false],
             [1, [], [2, 3]]
            ],
    [?_assert({ok, Expected} =:= ejson_encode:encode(Expected, []))
        || Expected <- Cases].

field_fun_test_() ->
    Enc = fun(Num) -> Num * 1000 end,
    Dec = fun(Num) -> Num div 1000 end,
    Opts = [{time, {field_fun, "jsTime", Enc, Dec}}],
    Record = {time, 2300},
    {ok, E} = ejson_encode:encode(Record, Opts),
    {ok, D} = ejson_decode:decode(E, Opts),
    ?_assert(Record =:= D).
