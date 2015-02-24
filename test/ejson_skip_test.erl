-module(ejson_skip_test).

-import(ejson_test_util, [json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

skip_test() ->
    Opts = [{request, skip, {string, "path"}, skip, {string, "method"}}],

    {ok, J} = ejson_encode:encode({request, "Socket info", "/index.html",
                                   self(), "GET"}, Opts),

    ?assertEqual(<<"/index.html">>, json_prop(J, "path")),
    ?assertEqual(<<"GET">>, json_prop(J, "method")),
    ?assertEqual(<<"request">>, json_prop(J, "__rec")),
    ?assertEqual(3, length(J)).
