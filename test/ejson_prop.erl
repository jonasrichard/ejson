-module(ejson_prop).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test() ->
    ?assertEqual(true, proper:quickcheck(camel_case_prop(), [{to_file, user}])).

basic() ->
    frequency([
            {1, integer()},
            {1, float()},
            {1, atom()},
            {1, tuple()},
            {1, binary()}
        ]).

descriptor() ->
    tuple([atom(), integer()]).

identifier_char() ->
    frequency([
              {$z - $a + 1, choose($a, $z)},
              {3, $_},
              {10, choose($0, $9)}
             ]).

record_name() ->
    ?LET(Chars, list(identifier_char()), list_to_atom(Chars)).

camel_case_prop() ->
    ?FORALL(Name, 
        ?SUCHTHAT(R, record_name(), ejson_util:is_convertable_atom(R)),
            begin
                CC = ejson_util:atom_to_binary_cc(Name),
                ejson_util:binary_to_atom_cc(CC) =:= Name
            end).

value_and_rule() ->
    ?LET(N, choose(0, 20),
         ?LET({Name, Fields, Values},
              {record_name(), vector(N, record_name()), vector(N, integer())},
              begin
                  {list_to_tuple([Name | Fields]),
                   list_to_tuple([Name | Values])}
              end
             )).

basic_prop() ->
    ?FORALL(Value, value_and_rule(),
            begin
                ?debugVal(Value),
                true
            end).
%%    ?FORALL(Value, basic(),
%%        ejson_decode:decode(ejson_encode:encode(Value, []), []) =:= Value).
