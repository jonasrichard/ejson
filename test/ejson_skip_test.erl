-module(ejson_skip_test).

-import(ejson_test_util, [json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

skip_test_() ->
    Rules = [{request, {skip, [{default, "INFO"}]},
                       {string, "path"},
                       skip,
                       {string, "method"}}],
    Record = {request, "Socket info", "/index.html", self(), "GET"},

    {ok, Json} = ejson:to_json(Record, Rules, []),
    {ok, D} = ejson:from_json(Json, request, Rules, []),

    [{"skip works during encoding",
      [?_assertEqual(<<"/index.html">>, json_prop(Json, "path")),
       ?_assertEqual(<<"GET">>, json_prop(Json, "method")),
       ?_assertEqual(2, length(jsx:decode(Json, [{return_maps, false}])))]},
     {"skip default during decoding",
      ?_assertMatch({request, "INFO", "/index.html", undefined, "GET"}, D)}].
