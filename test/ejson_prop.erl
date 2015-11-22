-module(ejson_prop).

-export([pre_conv/2,
         post_conv/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

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

%% Field name can be atom or string
field_rule_name() ->
    oneof([symb_name(), identifier()]).

rule() ->
    frequency([
               {1, {atom, field_rule_name()}},
               {1, {atom, field_rule_name(), [{default, atom()}]}},
               {1, {binary, field_rule_name()}},
               {1, {binary, field_rule_name(), [{default, binary()}]}},
               {1, {boolean, field_rule_name()}},
               {1, {boolean, field_rule_name(), [{default, boolean()}]}},
               {1, {number, field_rule_name()}},
               {1, {number, field_rule_name(), [{default, integer()}]}},
               {1, {string, field_rule_name()}},
               {1, {string, field_rule_name(),
                    [{pre_encode, {?MODULE, pre_conv}},
                     {post_decode, {?MODULE, post_conv}}]}},
               {1, {string, field_rule_name(), [{default, string()}]}},
               {1, {list, field_rule_name()}},      %% list of anything
               {1, skip},
               {1, {const, field_rule_name(), integer()}}
              ]).

record_rule() ->
    ?LET({RecordName, FieldRules},
         {symb_name(), list(rule())},
         begin
             list_to_tuple([RecordName | FieldRules])
         end).

%% function for rec_fun and field_fun
pre_conv(_Tuple, Value) ->
    Value.

post_conv(Value) ->
    Value.

proplist() ->
    list({symb_name(), integer()}).

basic(skip, _Rules, _Depth) ->
    undefined;
basic({atom, _Name}, _Rules, _Depth) ->
    atom();
basic({atom, _Name, _FieldOpts}, _Rules, _Depth) ->
    oneof([atom(), undefined]);
basic({binary, _Name}, _Rules, _Depth) ->
    binary();
basic({binary, _Name, _FieldOpts}, _Rules, _Depth) ->
    oneof([binary(), undefined]);
basic({boolean, _Name}, _Rules, _Depth) ->
    boolean();
basic({boolean, _Name, _FieldOpts}, _Rules, _Depth) ->
    oneof([boolean(), undefined]);
basic({string, _Name}, _Rules, _Depth) ->
    string();
basic({string, _Name, _FieldOpts}, _Rules, _Depth) ->
    oneof([string(), undefined]);
basic({list, _Name}, _Rules, 0) ->
    list(integer());
basic({list, Name}, Rules, Depth) ->
    frequency([
               {1, basic({list, Name}, Rules, 0)},
               {5, ?LAZY(non_empty(list(value(oneof(Rules), Rules, Depth - 1))))}
              ]);
basic({const, _Name, Const}, _Rules, _Depth) ->
    Const;
basic({number, _Name}, _Rules, _Depth) ->
    integer();
basic({number, _Name, _FieldOpts}, _Rules, _Depth) ->
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
                  {Simple, _} when Simple =:= atom orelse
                                   Simple =:= binary orelse
                                   Simple =:= boolean orelse
                                   Simple =:= number orelse
                                   Simple =:= string ->
                      Exp =:= Act;
                  {Simple, _, _} when Simple =:= atom orelse
                                      Simple =:= binary orelse
                                      Simple =:= boolean orelse
                                      Simple =:= number orelse
                                      Simple =:= string ->
                      Exp =:= Act;
                  {rec_fun, _, _} ->
                      true;
                  {field_fun, _, _, _} ->
                      true;
                  {const, _, _Value} ->
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
                        Type = element(1, Record),
                        {ok, Dec} = ejson_decode:decode(shuffle(Enc), Rules, Type),
%%                        ?debugVal(Dec),
                        equal(Record, Dec, Rules)
                end
            end)).

prop_camel_case() ->
    ?FORALL(Name, symb_name(),
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
