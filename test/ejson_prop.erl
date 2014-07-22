-module(ejson_prop).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DBG, begin dbg:start(), dbg:tracer(), dbg:tpl(ejson_prop, [{'_',[],[{return_trace}]}]), dbg:p(all, c) end).

all_test() ->
    %%dbg:start(), dbg:tracer(), dbg:tpl(ejson_prop, []), dbg:p(all, c),
    ?assertEqual(true, proper:quickcheck(prop_proplist_enc_dec(),
                                         [{to_file, user}, {numtests, 100}])).

pick_one(List) ->
    lists:nth(random:uniform(length(List)), List).

shuffle([]) ->
    [];
shuffle(List) ->
    Elem = pick_one(List),
    Rest = lists:delete(Elem, List),
    [Elem | shuffle(Rest)].

identifier_char() ->
    frequency([
              {$z - $a + 1, choose($a, $z)},
              {3, $_},
              {10, choose($0, $9)}
             ]).

identifier() ->
    ?SUCHTHAT(Name, [identifier_char()],
              ejson_util:is_name_convertable(Name)).

symb_name() ->
    ?LET(Id, identifier(), list_to_atom(Id)).

field_rule_name() ->
    oneof([symb_name(), identifier()]).

rule() ->
    frequency([
               {1, field_rule_name()},              %% number
               {1, {atom, field_rule_name()}},      %% atom
               {1, {string, field_rule_name()}},    %% utf-8 string
               {1, {binary, field_rule_name()}},    %% utf-8 binary
               {1, {list, field_rule_name()}},      %% list of anything
               {1, skip}
              ]).

record_rule() ->
    ?LET({RecordName, FieldRules},
         {symb_name(), [rule()]},
         begin
             list_to_tuple([RecordName | FieldRules])
         end).

basic_value(skip, _) ->
    undefined;
basic_value({atom, _}, _) ->
    atom();
basic_value({string, _}, _) ->
    string();
basic_value({binary, _}, _) ->
    binary();
basic_value({list, _}, Rules) ->
    ?LAZY(
        frequency([
            {1, []},
            {1, record_list(Rules)}
          ]));
basic_value(_, _) ->
    frequency([{1, integer()}, {1, float()}]).

record_value() ->
    ?LET(Rules, non_empty(list(record_rule())), record_value(Rules)).

record_value(Rules) ->
    Rule = pick_one(Rules),
    [RecordName | Fields] = tuple_to_list(Rule),
    FieldGens = [X || X <- [basic_value(Field, Rules) || Field <- Fields],
                      X =/= undefined],
    
    ?LET(Values, FieldGens,
         begin
             {list_to_tuple([RecordName | Values]), Rules}
         end).

record_list(Rules) ->
    ?LET(List, list(record_value(Rules)),
         begin
             [Record || {Record, _Opts} <- List]
         end).

proplist() ->
    list({symb_name(), integer()}).

prop_encode_decode() ->
    ?FORALL({Record, Opts}, record_value(),
            begin
                ?debugVal(Record),
                ?debugVal(Opts),
                case ejson_encode:encode(Record, Opts) of
                    {error, duplicate_record_names} ->
                        ?debugVal(Opts),
                        true;
                    Enc ->
                        ?debugVal(Enc),
                        Dec = ejson_decode:decode(shuffle(Enc), Opts),
                        ?debugVal(Dec),
                        Record =:= Dec
                end
            end).

prop_proplist_enc_dec() ->
    ?FORALL(PropList, proplist(),
            begin
                Rules = [{simple, {proplist, "properties"}}],
                Record = {simple, PropList},
                ?debugVal(Record),
                case ejson_encode:encode(Record, Rules) of
                    {error, duplicate_property} ->
                        true;
                    Enc ->
                        ?debugVal(Enc),
                        {simple, D} = ejson_decode:decode(shuffle(Enc), Rules),
                        ?debugVal(D),
                        lists:all(
                          fun(E) ->
                                  lists:member(E, D)
                          end, PropList)
                end
            end).

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
