-module(ejson_trans_test).

-compile({parse_transform, ejson_trans}).

-include_lib("eunit/include/eunit.hrl").

-json({square, "side"}).

all_test_() ->
    Record = {square, 50},
    Json = to_json(Record),
    ?debugVal(Json),
    Square = from_json(Json),
    ?debugVal(Square),
    ?_assert(Record =:= Square).
