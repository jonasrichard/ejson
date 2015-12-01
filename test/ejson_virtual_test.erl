-module(ejson_virtual_test).

-export([wealth_jsx/1, jsx_wealth/2]).

-import(ejson_test_util, [json_prop/2, json_path/2]).

-include_lib("eunit/include/eunit.hrl").

wealth_jsx(Record) ->
    {person, Age, Salary} = Record,
    (Age - 23) * Salary.

jsx_wealth(Record, _Value) ->
    [Age, Salary] = Record,
    [Age, Salary + 1000].

add_field_test_() ->
    Rules = [{person, {number, age}, {number, salary},
                      {virtual, wealth, [{pre_encode, {?MODULE, wealth_jsx}},
                                         {post_decode, {?MODULE, jsx_wealth}}]}}],
    Record = {person, 40, 10000},

    {ok, J} = ejson_encode:encode(Record, Rules, []),
    ?debugVal(J),
    {ok, D} = ejson_decode:decode(J, person, Rules, []),
    ?debugVal(D),
    [?_assertEqual(170000, json_path(J, "wealth")),
     ?_assertMatch({person, 40, 11000}, D)].
