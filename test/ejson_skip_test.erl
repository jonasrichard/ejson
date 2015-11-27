-module(ejson_skip_test).

-import(ejson_test_util, [json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

skip_test() ->
    Rules = [{request, {skip, [{default, "INFO"}]},
                       {string, "path"},
                       skip,
                       {string, "method"}}],
    Record = {request, "Socket info", "/index.html", self(), "GET"},

    {ok, Json} = ejson:to_json(Record, Rules, []),

    Decoded = jsx:decode(Json),
    ?assertEqual(<<"/index.html">>, json_prop(Decoded, "path")),
    ?assertEqual(<<"GET">>, json_prop(Decoded, "method")),
    ?assertEqual(2, length(Decoded)),
    
    {ok, D} = ejson:from_json(Json, request, Rules, []),
    ?assertMatch({request, "INFO", "/index.html", undefined, "GET"}, D).
