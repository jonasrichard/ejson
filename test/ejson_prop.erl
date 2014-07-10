-module(ejson_prop).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

all_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}])).

identifier_char() ->
    frequency([
              {$z - $a + 1, choose($a, $z)},
              {3, $_},
              {10, choose($0, $9)}
             ]).

record_name() ->
    ?LET(Name,
        ?SUCHTHAT(Chars,
                  list(identifier_char()),
                  ejson_util:is_convertable_atom(list_to_atom(Chars))),
        list_to_atom(Name)).

basic_rule_value() ->
    frequency([
            %% Some basic types don't need meta info
            {1, {record_name(), integer()}},
            {1, {record_name(), float()}},
            {1, {{atom, record_name()}, atom()}}
        ]).

value_and_rule() ->
    ?LET(N, choose(0, 20),
         ?LET({Name, RuleValues},
              {record_name(), vector(N, basic_rule_value())},
              begin
                  {Fields, Values} = lists:unzip(RuleValues),
                  {list_to_tuple([Name | Fields]),
                   list_to_tuple([Name | Values])}
              end
             )).

prop_camel_case() ->
    ?FORALL(Name, 
        ?SUCHTHAT(R, record_name(), ejson_util:is_convertable_atom(R)),
            begin
                CC = ejson_util:atom_to_binary_cc(Name),
                ejson_util:binary_to_atom_cc(CC) =:= Name
            end).

prop_zip() ->
    ?FORALL({A, B}, {list(), list()},
            begin
                Zip = ejson_util:zip(A, B),
                length(Zip) =:= erlang:max(length(A), length(B))
            end).

prop_basic() ->
    ?FORALL({Opt, Record}, value_and_rule(),
            begin
                Enc = ejson_encode:encode(Record, [Opt]),
                case Enc of
                    {error, {duplicate_field_name, _}} ->
                        true;
                    _ ->
                        Dec = ejson_decode:decode(Enc, [Opt]),
                        if Record =:= Dec -> true;
                           true ->
                               begin
                                   ?debugVal(Opt),
                                   ?debugVal(Record),
                                   ?debugVal(Enc),
                                   ?debugVal(Dec),
                                   false
                               end
                        end
                end
            end).
