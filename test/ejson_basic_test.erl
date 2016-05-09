-module(ejson_basic_test).

-export([enc_fun/2,
         dec_fun/1]).

-import(ejson_test_util, [json_prop/2, json_path/2, jsx_prop/2, jsx_path/2]).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

-json({time, {number, "jsTime", [{pre_encode, {?MODULE, enc_fun}},
                                 {post_decode, {?MODULE, dec_fun}}]}}).
-json({message, {binary, body, [base64]}}).

value_test_() ->
    [{"Encode integer value",
        ?_assertEqual({ok, <<"1">>}, to_json(1))},
     {"Encode real value",
        ?_assertEqual({ok, <<"3.4">>}, to_json(3.4))},
     {"Decode integer value",
        ?_assertEqual({ok, 40}, from_json(<<"40">>))},
     {"Decode real value",
        ?_assertEqual({ok, 9.998877}, from_json(<<"9.998877">>))}].

null_test_() ->
    [{"Encode null value", ?_assertEqual({ok, <<"null">>}, to_json(undefined))},
     {"Decode null value", ?_assertEqual({ok, undefined}, from_json(<<"null">>))}].

list_test_() ->
    [{"Encode empty list",
        ?_assertEqual({ok, <<"[]">>}, to_json([]))},
     {"Decode empty list",
        ?_assertEqual({ok, []}, from_json(<<"[]">>))},
     {"Encode integer list",
        ?_assertEqual({ok, <<"[1,2]">>}, to_json([1, 2]))},
     {"Decode integer list",
        ?_assertEqual({ok, [1, 2]}, from_json(<<"[1,2]">>))}
    ].

base64_test_() ->
    [{"Base64 encode binaries",
        ?_assertEqual({ok, <<"{\"body\":\"V2UgYXJlIHNpbmtpbmch\"}">>},
                      to_json({message, <<"We are sinking!">>}))},
     {"Base64 decode binaries",
        ?_assertEqual({ok, {message, <<"S3cr3t">>}},
                      from_json(<<"{\"body\":\"UzNjcjN0\"}">>, message))}].

enc_fun(_Rec, Num) -> Num * 1000.
dec_fun(Num) -> Num div 1000.

pre_post_callback_test_() ->
    Record = {time, 2300},
    {ok, E} = to_json(Record),
    {ok, D} = from_json(E, time),
    [{"Encode record millisec", ?_assertEqual(2300000, json_prop(E, "jsTime"))},
     {"Not encode record type", ?_assertEqual(undefined, json_prop(E, "__rec"))},
     {"Decode record", ?_assertEqual(Record, D)}].

-define(TYPE(Val, Rules, Err),
        {lists:flatten(io_lib:format("~p error test", [Err])),
            ?_assertEqual(Exp(Val, Err), Enc(Val, Rules))}).

type_fail_test_() ->
    Rules1 = [{request, {atom, "method"}}],
    Rules2 = [{request, {binary, "method"}}],
    Rules3 = [{request, {list, "method"}}],

    Enc = fun(V, O) -> ejson_encode:encode({request, V}, O, []) end,
    Exp = fun(V, E) -> {error, {E, "method", V}} end,

    %% Test when atom is expected but other data are passed
    [?TYPE("string", Rules1, atom_value_expected),
     ?TYPE(1, Rules1, atom_value_expected),
     ?TYPE(<<"apple">>, Rules1, atom_value_expected),
     ?TYPE("test", Rules2, binary_value_expected),
     ?TYPE(3, Rules2, binary_value_expected),
     ?TYPE(1, Rules3, list_value_expected)].

embedded_record_test_() ->
    Rules = [{person, {string, name}, {record, address}},
             {address, {string, city}, {string, country}}],
    Rec = {person, "Joe", {address, "Budapest", "Hun"}},
    {ok, Enc} = ejson_encode:encode(Rec, Rules, []),
    {ok, Dec} = ejson_decode:decode(Enc, person, Rules, []),
    [{"String field",
        ?_assertEqual(<<"Joe">>, jsx_prop(Enc, "name"))},
     {"Embedded field",
        [?_assertEqual(<<"Budapest">>, jsx_path(Enc, "address.city")),
         ?_assertEqual(<<"Hun">>, jsx_path(Enc, "address.country"))]},
     {"__rec field present",
        ?_assertEqual(<<"address">>, jsx_path(Enc, "address.__rec"))},
     {"Whole record equality",
        ?_assertMatch({person, _, {address, "Budapest", "Hun"}}, Dec)}].

typed_record_test_() ->
    Rules = [{person, {string, name}, {record, address, [{type, address}]}},
            {address, {string, city}, {string, country}}],
    Rec = {person, "Joe", {address, "Budapest", "Hun"}},
    {ok, J} = ejson:to_json(Rec, Rules, []),
    {"Untyped record has no __rec",
        ?_assertEqual(undefined, json_path(J, "address.__rec"))}.

