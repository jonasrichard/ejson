-module(ejson_test).

-include_lib("eunit/include/eunit.hrl").

-json({rectangle, "aSide", b_side}).

json_props_test_() ->
    Opts = ejson:json_props([?MODULE]),
    [?_assert(1 =:= length(Opts)),
     ?_assert({rectangle, "aSide", b_side} =:= hd(Opts))].
