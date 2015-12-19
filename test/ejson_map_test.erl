-module(ejson_map_test).

-export([to_jsx/2, from_jsx/1]).

-include_lib("eunit/include/eunit.hrl").

-import(ejson_test_util, [json_path/2]).

map_test_() ->
    Rules = [{address, {map, data, [{pre_encode, {?MODULE, to_jsx}},
                                    {post_decode, {?MODULE, from_jsx}}]}}],
    Address = {address, #{country => "Hungary",
                          city => "Budapest",
                          zip => "1085",
                          street => "Szigony str 5."}},

    {ok, E} = ejson:to_json(Address, Rules, []),
    ?debugVal(E),
    {ok, D} = ejson:from_json(E, address, Rules, []),

    [{"Map has 4 items",
        ?_assertEqual(4, length(json_path(E, "data")))},
     {"Map key test",
        ?_assertEqual(<<"Budapest">>, json_path(E, "data.city"))},
     {"Decoded data is a map",
        ?_assert(is_map(element(2, D)))},
     {"Zip code decoded",
        ?_assertMatch({address, #{zip := "1085"}}, D)}
    ].

to_jsx(_Record, Map) ->
    maps:fold(
      fun(K, V, Acc) ->
              [{atom_to_binary(K, utf8), list_to_binary(V)} | Acc]
      end, [], Map).

from_jsx(Jsx) ->
    lists:foldl(
      fun({K, V}, Acc) ->
              maps:put(binary_to_atom(K, utf8), binary_to_list(V), Acc)
      end, #{}, Jsx).
