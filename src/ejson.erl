-module(ejson).

-export([
        to_json/2,
        to_json_module/2,
        to_json_modules/2,
        json_props/1
    ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% TODO: float conversion?
%% now the decimals is wired to 7

%%%============================================================================
%%% External API functions
%%%============================================================================

to_json_modules(Term, ModuleList) ->
    Opts = json_props(ModuleList),
    
    to_json(Term, Opts).

to_json_module(Term, Module) ->
    %% Get -json attributes from module info
    Opts = json_props([Module]),

    %% Call to_json with the Options we got
    to_json(Term, Opts).

json_props(ModuleList) ->
    lists:foldl(
        fun(Module, Acc) ->
            Attrs = proplists:get_value(attributes, Module:module_info()),
            Opts = lists:flatten([V || {json, V} <- Attrs]),

            Opts ++ Acc
        end, [], ModuleList).

%%-----------------------------------------------------------------------------
%% @doc Convert Term to json with the Options passed.
%%
%% It converts
%%   - numbers to binaries
%%   - undefined, true, false to null, true, false respectively
%%   - strings to binaries
%%   - {list, FieldName} to list of json objects (calling conv)
%%   - skip to skip attribute serialization
%%   - {pre, FieldName, Fun} to pre-process data with Fun. Fun(Tuple) will be
%%     called to convert a value, or create a new extra attribute.
%%   - tuples (record representions) to list of name/value pair according
%%     to the rules in the Options.
%%
%% Options can be:
%%   - {RecordName, [FieldNames]}
%%     It can convert a record to name/value pairs. If there is an option
%%     {person, "nickName", "numberOfPapers"} in the Options, then a
%%     {person, "Jim", 120} will be converted into a list of name/value
%%     pairs like
%%     [{<<"nickName">>, <<"Jim">>}, {<<"numberOfPapers">>, <<"120">>}].
%%
%%     Later it can be feed into jsx:encode().
%%
%%   - FieldName can be {list, Name}. In that case the field in the tuple
%%     will be treated as a list.
%%
%% @end
%%-----------------------------------------------------------------------------
to_json(Term, Options) ->
    Enc = conv(Term, Options),
    jsx:encode(Enc).

conv(Tuple, Options) when is_tuple(Tuple) ->
    %% Get record name and values from the tuple
    [RecName | Vals] = erlang:tuple_to_list(Tuple),
    
    %% Get the options for that record
    FieldNames = proplists:get_value(RecName, Options),

    %% Convert each values
    lists:reverse(
        lists:foldl(
            fun({Rule, Value}, Acc) ->
                case apply_rule(Rule, Tuple, Value, Options) of
                    {AttrName, AttrValue} ->
                        [{list_to_binary(AttrName), AttrValue} | Acc];
                    undefined ->
                        Acc
                end
            end,
            [],
            zip(FieldNames, Vals)));
conv(String, _) when is_list(String) ->
    list_to_binary(String);
conv(Binary, _) when is_binary(Binary) ->
    Binary;
conv(Number, _) when is_number(Number) ->
    Number;
conv(undefined, _Name) ->
    null;
conv(Bool, _) when is_boolean(Bool) ->
    Bool;
conv(Atom, _) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1);
conv(Pid, _) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).


conv_list(List, Options) when is_list(List) ->
    [conv(L, Options) || L <- List];
conv_list(List, _Options) ->
    throw({error, {not_a_list, List}}).

apply_rule({list, Name}, _Record, Value, Options) ->
    List = conv_list(Value, Options),
    {Name, List};
apply_rule({proplist, Name}, _Record, Value, Options) ->
    PropList = conv_proplist(Value, Options),
    {Name, PropList};
apply_rule(skip, _Record, _Value, _Options) ->
    undefined;
apply_rule({field_fun, Name, {M, F}}, _Record, Value, Options) ->
    Value2 = erlang:apply(M, F, [Value]),
    {Name, conv(Value2, Options)};
apply_rule({rec_fun, Name, {M, F}}, Record, _Value, Options) ->
    Value2 = erlang:apply(M, F, [Record]),
    {Name, conv(Value2, Options)};
apply_rule({const, Name, Const}, _Record, _Value, Options) ->
    {Name, conv(Const, Options)};
apply_rule(Name, _Record, Value, Options) ->
    {Name, conv(Value, Options)}.

%%-----------------------------------------------------------------------------
%% @doc
%% Convert proplist to json object.
%%
%% It deals with [{prop_name, Value}, bool_prop, ...] lists. Iterates over
%% each key, convert the atom key into camel-cased name. 
%% @end
%%-----------------------------------------------------------------------------
conv_proplist(List, Options) when is_list(List) ->
    Keys = proplists:get_keys(List),
    lists:map(
        fun(Key) ->
            case proplists:get_all_values(Key, List) of
                [Value] ->
                    {camel_case(Key), conv(Value, Options)};
                Vals when length(Vals) > 1 ->
                    {camel_case(Key), conv_list(Vals, Options)}
            end
        end,
        Keys);
conv_proplist(List, _Options) ->
    throw({error, {not_a_proplist, List}}).

camel_case(Atom) ->
    list_to_binary(lists:reverse(camel_case(atom_to_list(Atom), []))).

camel_case([], R) ->
    R;
camel_case([$_, L | T], R) ->
    camel_case(T, [string:to_upper(L) | R]);
camel_case([H | T], R) ->
    camel_case(T, [H | R]).

zip([], []) ->
    [];
zip([H1|T1], []) ->
    [{H1, undefined} | zip(T1, [])];
zip([], [H2|T2]) ->
    [{undefined, H2} | zip([], T2)];
zip([H1|T1], [H2|T2]) ->
    [{H1, H2} | zip(T1, T2)].


%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

%%zip_test() ->
%%    ?assertEqual([{undefined, 1}], zip([], [1])),
%%    ?assertEqual([{2, 1}, {3, undefined}], zip([2, 3], [1])),
%%    ?assertEqual([], zip([], [])),
%%    ?assertEqual([{1, 2}, {3, 4}], zip([1, 3], [2, 4])).
%%
%%camel_case_test() ->
%%    ?assertEqual(<<"simple">>, camel_case(simple)),
%%    ?assertEqual(<<"xAxis">>, camel_case(x_axis)).
%%
%%conv_atom_test() ->
%%    Options = [{weather, ["month", "value"]}],
%%    
%%    J = conv({weather, january, snowing}, Options),
%%    
%%    ?assertEqual(<<"january">>, json_prop(J, "month")),
%%    ?assertEqual(<<"snowing">>, json_prop(J, "value")).
%%
%%conv_pid_test() ->
%%    Options = [{procs, ["sup"]}],
%%    Bin = list_to_binary(pid_to_list(self())),
%%
%%    J = conv({procs, self()}, Options),
%%
%%    ?assertEqual(Bin, json_prop(J, "sup")).
%%
%%undef_test() ->
%%    Options = [{nullable, ["object"]}],
%%
%%    J = conv({nullable, undefined}, Options),
%%    ?assertEqual(null, json_prop(J, "object")).
%%
%%bool_test() ->
%%    Options = [{bool, ["value"]}],
%%
%%    J1 = conv({bool, true}, Options),
%%    J2 = conv({bool, false}, Options),
%%
%%    ?assertEqual(true, json_prop(J1, "value")),
%%    ?assertEqual(false, json_prop(J2, "value")).
%%
%%number_test() ->
%%    Options = [{complex, ["r", "i"]}],
%%
%%    J = conv({complex, -35, 809}, Options),
%%
%%    ?assertEqual(-35, json_prop(J, "r")),
%%    ?assertEqual(809, json_prop(J, "i")).
%%
%%binary_test() ->
%%    Options = [{text, ["value"]}],
%%
%%    J = conv({text, <<"A binary as is">>}, Options),
%%
%%    ?assertEqual(<<"A binary as is">>, json_prop(J, "value")).
%%
%%string_test() ->
%%    Options = [{message, ["text"]}],
%%
%%    J = conv({message, "Texting something"}, Options),
%%
%%    ?assertEqual(<<"Texting something">>, json_prop(J, "text")).

-endif.
