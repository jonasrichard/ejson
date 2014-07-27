-module(ejson_prop).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-type record_name()     :: atom().

-define(DBG, begin dbg:start(), dbg:tracer(), dbg:tpl(ejson_prop, [{'_',[],[{return_trace}]}]), dbg:p(all, c) end).

all_test() ->
    dbg:start(), dbg:tracer(),
    dbg:tpl(ejson_prop, basic, 3, []), %% [{'_', [], [{return_trace}]}]),
    dbg:p(all, c),
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
               {1, {field_fun, field_rule_name(), {?MODULE, conv}}},
               {1, {rec_fun, field_rule_name(), {?MODULE, conv}}},
               {1, {const, field_rule_name(), integer()}}
              ]).

record_rule() ->
    ?LET({RecordName, FieldRules},
         {symb_name(), [rule()]},
         begin
             list_to_tuple([RecordName | FieldRules])
         end).

%% function for rec_fun and field_fun
conv(_) ->
    1.

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
            {1, ?LAZY(record_list(Rules))}
          ]));
basic_value({field_fun, _, _}, _) ->
    conv(undefined);
basic_value({rec_fun, _, _}, _) ->
    conv(undefined);
basic_value({const, _, X}, _) ->
    X;
basic_value(_, Rules) ->
    ?LAZY(frequency([
               {1, integer()},
               {1, float()},
               {1, ?LET({Val, _}, record_value(Rules), Val)}
              ])).

record_value() ->
    ?LAZY(?LET(Rules,
               non_empty(resize(5, list(record_rule()))),
               record_value(Rules))).

record_value(Rules) ->
    Rule = pick_one(Rules),
    [RecordName | Fields] = tuple_to_list(Rule),
    FieldGens = [basic_value(Field, Rules) || Field <- Fields],
    
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

%%----

basic(skip, _Rules, _Depth) ->
    undefined;
basic({atom, _Name}, _Rules, _Depth) ->
    atom();
basic({binary, _Name}, _Rules, _Depth) ->
    binary();
basic({string, _Name}, _Rules, _Depth) ->
    string();
basic({list, _Name}, _Rules, 0) ->
    [];
basic({list, _Name}, Rules, Depth) ->
    frequency([
               {1, []},
               {5, ?LAZY(list(value(pick_one(Rules), Rules, Depth - 1)))}
              ]);
basic({const, _Name, Const}, _Rules, _Depth) ->
    Const;
basic({rec_fun, _Name, {_M, _F}}, _Rules, _Depth) ->
    integer();
basic({field_fun, _Name, {_M, _F}}, _Rules, _Depth) ->
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

%%----

prop_gen() ->
    ?FORALL(Rules, non_empty(resize(5, list(record_rule()))),
        begin
        ?debugVal(Rules),
        ?FORALL(Record, value(pick_one(Rules), Rules),
                begin
                    ?debugVal(Record),
                    true
                end)
        end).


pro_encode_decode() ->
    ?FORALL({Record, Opts}, record_value(),
            begin
                ?debugVal(Record),
                ?debugVal(Opts),
                true
%%                case ejson_encode:encode(Record, Opts) of
%%                    {error, duplicate_record_names} ->
%%                        ?debugVal(Opts),
%%                        true;
%%                    Enc ->
%%                        ?debugVal(Enc),
%%                        Dec = ejson_decode:decode(shuffle(Enc), Opts),
%%                        ?debugVal(Dec),
%%                        Record =:= Dec
%%                end
            end).

pro_proplist_enc_dec() ->
    ?FORALL(PropList, proplist(),
            begin
                Rules = [{simple, {proplist, "properties"}}],
                Record = {simple, PropList},
                %%?debugVal(Record),
                case ejson_encode:encode(Record, Rules) of
                    {error, duplicate_property} ->
                        true;
                    Enc ->
                        %%?debugVal(Enc),
                        {simple, D} = ejson_decode:decode(shuffle(Enc), Rules),
                        %%?debugVal(D),
                        lists:all(
                          fun(E) ->
                                  lists:member(E, D)
                          end, PropList)
                end
            end).

pro_camel_case() ->
    ?FORALL(Name, 
        ?SUCHTHAT(R, record_name(), ejson_util:is_name_convertable(R)),
            begin
                CC = ejson_util:atom_to_binary_cc(Name),
                ejson_util:binary_to_atom_cc(CC) =:= Name
            end).

pro_zip() ->
    ?FORALL({A, B}, {list(), list()},
            begin
                Zip = ejson_util:zip(A, B),
                length(Zip) =:= erlang:max(length(A), length(B))
            end).
