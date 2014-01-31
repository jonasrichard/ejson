-module(ejson_preprocess_test).

-ifdef(TEST).

-import(ejson, [conv/2, json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

field_fun_enc_test() ->
    Opts = [{rect, [{field_fun, "a", {?MODULE, rect_field_conv}},
                    {field_fun, "b", {?MODULE, rect_field_conv}}]
            }],

    J = conv({rect, 3, 5}, Opts),

    ?assertEqual(30, json_prop(J, "a")),
    ?assertEqual(50, json_prop(J, "b")).

rec_fun_enc_test() ->
    Opts = [{rect, ["a", "b", {rec_fun, "area", {?MODULE, rect_area}}]}],

    J= conv({rect, 8, 12}, Opts),

    ?assertEqual(8, json_prop(J, "a")),
    ?assertEqual(12, json_prop(J, "b")),
    ?assertEqual(96, json_prop(J, "area")).

const_test() ->
    Opts = [{dummy, [{const, "file", {file, "erl", "sh"}}]},
            {file, ["name", "ext"]}],

    J = conv({dummy, "whatever"}, Opts),
    File = json_prop(J, "file"),

    ?assertEqual(<<"erl">>, json_prop(File, "name")),
    ?assertEqual(<<"sh">>, json_prop(File, "ext")).

rect_field_conv(A) ->
    A * 10.

rect_area({rect, A, B}) ->
    A * B.

-endif.
