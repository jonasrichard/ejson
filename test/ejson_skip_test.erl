-module(ejson_skip_test).

-ifdef(TEST).

-import(ejson, [conv/2, json_prop/2]).

-include_lib("eunit/include/eunit.hrl").

skip_test() ->
    Opts = [{request, [skip, "path", skip, "method"]}],

    J = conv({request, "Socket info", "/index.html", self(), "GET"}, Opts),

    ?assertEqual(<<"/index.html">>, json_prop(J, "path")),
    ?assertEqual(<<"GET">>, json_prop(J, "method")),
    ?assertEqual(2, length(J)).

-endif.
