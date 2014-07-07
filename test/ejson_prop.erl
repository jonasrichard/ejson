-module(ejson_prop).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test() ->
    ?assertEqual(true, proper:quickcheck(basic_prop(), [{to_file, user}])).

basic() ->
    frequency([
            {1, integer()},
            {1, float()},
            {1, atom()},
            {1, tuple()},
            {1, binary()}
        ]).

basic_prop() ->
    ?FORALL(Value, basic(),
        ejson_decode:decode(ejson_encode:encode(Value, []), []) =:= Value).
