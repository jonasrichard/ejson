-module(ejson_prop).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type record_name()     :: atom().

all_test() ->
    ?assertEqual([], proper:module(?MODULE, [{to_file, user}])).

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
               {1, skip},
               {1, {field_fun, field_rule_name(), {?MODULE, fconv},
                                                  {?MODULE, fconv}}},
               {1, {rec_fun, field_rule_name(), {?MODULE, rconv}}},
               {1, {const, field_rule_name(), integer()}}
              ]).

record_rule() ->
    ?LET({RecordName, FieldRules},
         {symb_name(), list(rule())},
         begin
             list_to_tuple([RecordName | FieldRules])
         end).

%% function for rec_fun and field_fun
fconv(X) ->
    X.

rconv(_) ->
    1.

proplist() ->
    list({symb_name(), integer()}).

basic(skip, _Rules, _Depth) ->
    undefined;
basic({atom, _Name}, _Rules, _Depth) ->
    atom();
basic({binary, _Name}, _Rules, _Depth) ->
    binary();
basic({string, _Name}, _Rules, _Depth) ->
    string();
basic({list, _Name}, _Rules, 0) ->
    list(integer());
basic({list, Name}, Rules, Depth) ->
    frequency([
               {1, basic({list, Name}, Rules, 0)},
               {5, ?LAZY(non_empty(list(value(oneof(Rules), Rules, Depth - 1))))}
              ]);
basic({const, _Name, Const}, _Rules, _Depth) ->
    Const;
basic({rec_fun, _Name, {_M, _F}}, _Rules, _Depth) ->
    integer();
basic({field_fun, _Name, _MF1, _MF2}, _Rules, _Depth) ->
    integer();
basic(Name, _Rules, _Depth) when is_list(Name) orelse is_atom(Name) ->
    integer().

value(RecordRule, Rules) ->
    ?SIZED(S, value(RecordRule, Rules, S)).

value(RecordRule, Rules, Depth) ->
    [RecordName | FieldRules] = tuple_to_list(RecordRule),
    FieldGens = [basic(FieldRule, Rules, Depth) || FieldRule <- FieldRules],

    ?LET(Values, FieldGens,
         begin
             list_to_tuple([RecordName | Values])
         end).

equal(Expected, Actual, Opts) ->
    [RecordName | Exps] = tuple_to_list(Expected),
    Fields = ejson_util:get_fields(RecordName, Opts),
    [RecordName | Acts] = tuple_to_list(Actual),
    lists:all(
      fun({Exp, Act, Rule}) ->
%%              ?debugVal(Rule),
%%              ?debugVal(Exp),
%%              ?debugVal(Act),
              case Rule of
                  skip ->
                      true;
                  Name when is_atom(Name) orelse is_list(Name) ->
                      Exp =:= Act;
                  {Simple, _} when Simple =:= atom orelse
                                   Simple =:= binary orelse
                                   Simple =:= string ->
                      Exp =:= Act;
                  {rec_fun, _, _} ->
                      true;
                  {field_fun, _, _, _} ->
                      true;
                  {const, _, Value} ->
                      true;
                  {list, _} ->
                      Exp =:= Act 
              end
      end, lists:zip3(Exps, Acts, Fields)).

prop_encode_decode() ->
    ?FORALL(Rules, non_empty(resize(5, list(record_rule()))),
        ?FORALL(Record, value(pick_one(Rules), Rules, 0),
            begin
%%                ?debugVal(Rules),
%%                ?debugVal(Record),
                case ejson_encode:encode(Record, Rules) of
                    {error, {duplicate_records, R}} when is_list(R) ->
                        true;
                    {error, {duplicate_fields, F}} when is_list(F) ->
                        true;
                    {ok, Enc} ->
%%                        ?debugVal(Enc),
                        {ok, Dec} = ejson_decode:decode(shuffle(Enc), Rules),
%%                        ?debugVal(Dec),
                        equal(Record, Dec, Rules)
                end
            end)).

pro_proplist_enc_dec() ->
    ?FORALL(PropList, proplist(),
            begin
                Rules = [{simple, {proplist, "properties"}}],
                Record = {simple, PropList},
                %%?debugVal(Record),
                case ejson_encode:encode(Record, Rules) of
                    {error, duplicate_property} ->
                        true;
                    {ok, Enc} ->
                        %%?debugVal(Enc),
                        {simple, D} = ejson_decode:decode(shuffle(Enc), Rules),
                        %%?debugVal(D),
                        lists:all(
                          fun(E) ->
                                  lists:member(E, D)
                          end, PropList)
                end
            end).

prop_camel_case() ->
    ?FORALL(Name, 
        ?SUCHTHAT(R, record_name(), ejson_util:is_name_convertable(R)),
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
