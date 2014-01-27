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
            fun({{list, Name}, Value}, Acc) ->
                [{list_to_binary(Name), conv_list(Value, Options)} | Acc];
               ({{proplist, Name}, Value}, Acc) ->
                [{list_to_binary(Name), conv_proplist(Value, Options)} | Acc];
               ({skip, _Value}, Acc) ->
                Acc;
               ({{pre, Name, {Mod, Fun}}, _Value}, Acc) ->
                Value = erlang:apply(Mod, Fun, [Tuple]),
                [{list_to_binary(Name), conv(Value, Options)} | Acc];
               ({Name, Value}, Acc) ->
                [{list_to_binary(Name), conv(Value, Options)} | Acc]
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


conv_list(List, Options) ->
    [conv(L, Options) || L <- List].


%%-----------------------------------------------------------------------------
%% @doc
%% Convert proplist to json object.
%%
%% It deals with [{prop_name, Value}, bool_prop, ...] lists. Iterates over
%% each key, convert the atom key into camel-cased name. 
%% @end
%%-----------------------------------------------------------------------------
conv_proplist(List, Options) ->
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
        Keys).

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

%%number_json_test() ->
%%    ?assertEqual(<<"1">>, conv(1, [])),
%%    ?assertEqual(<<"-83">>, conv(-83, [])),
%%    ?assertEqual(<<"1.2">>, conv(1.2, [])),
%%    ?assertEqual(<<"0.07">>, conv(7.0e-2, [])).

atom_json_test() ->
    ?assertEqual(null, conv(undefined, [])),
    ?assertEqual(false, conv(false, [])),
    ?assertEqual(true, conv(true, [])),
    ?assertEqual(<<"node_1">>, conv(node_1, [])).

record_test() ->
    Rec = {person, "Joe", 56000, true},
    Options = [{person, ["name", "salary", "hasBooks"]}],

    ?assertEqual([{<<"name">>, <<"Joe">>},
                  {<<"salary">>, 56000},
                  {<<"hasBooks">>, true}],
                 conv(Rec, Options)).

record_list_test() ->
    Book1 = {book, "Introduction to clean coding", 251},
    Book2 = {book, "TDD - the easy way", 760},
    Person = {person, "Sam", [Book1, Book2]},
    
    Options = [
        {book, ["title", "numberOfPages"]},
        {person, ["nickName", {list, "books"}]}], 
    
    C = conv(Person, Options),

    [Pers, Books] = C,

    ?assertEqual({<<"nickName">>, <<"Sam">>}, Pers),
    ?assertEqual({<<"books">>, [
            [{<<"title">>, <<"Introduction to clean coding">>},
             {<<"numberOfPages">>, 251}],
            [{<<"title">>, <<"TDD - the easy way">>},
             {<<"numberOfPages">>, 760}]
        ]}, Books).

proplist_test() ->
    Square = {shape, square, [{a, 10}]},
    Circle = {shape, circle, [{radius, 5}]},
    Rect = {shape, rect, [{x_left, 10}, {y_left, 15},
                          {x_right, 50}, {y_right, 30}]},

    Options = [{shape, ["type", {proplist, "data"}]}],

    ?debugVal(to_json(Rect, Options)).

pid_test() ->
    Req = {request, self()},
    Options = [{request, ["pid"]}],

    C = conv(Req, Options),

    Self = list_to_binary(pid_to_list(self())),
    ?assertEqual({<<"pid">>, Self}, hd(C)).

skip_test() ->
    Req = {request, self(), socket, "Message"},
    Options = [{request, [skip, skip, "message"]}],

    C = conv(Req, Options),
    ?assertEqual({<<"message">>, <<"Message">>}, hd(C)).

fun_test() ->
    Area = fun({square, Side}) ->
                   Side * Side
           end,
    Square = {square, 5},
    Options = [{square, ["side", {pre, "area", Area}]}],

    C = conv(Square, Options),
    ?assertEqual(proplists:get_value(<<"side">>, C), 5),
    ?assertEqual(proplists:get_value(<<"area">>, C), 25).

-endif.
