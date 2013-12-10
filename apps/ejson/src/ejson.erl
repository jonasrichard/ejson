-module(ejson).

-export([
        to_json/2
    ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% TODO: float conversion?
%% now the decimals is wired to 7

%%%============================================================================
%%% External API functions
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Convert Term to json with the Options passed.
%%
%% It converts
%%   - numbers to binaries
%%   - undefined, true, false to null, true, false respectively
%%   - strings to binaries
%%   - {list, FieldName} to list of json objects (calling conv)
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
    lists:map(
        fun({{list, Name}, Value}) ->
            {list_to_binary(Name), conv_list(Value, Options)};
           ({Name, Value}) ->
            {list_to_binary(Name), conv(Value, Options)}
        end,
        lists:zip(FieldNames, Vals));
conv(String, _) when is_list(String) ->
    list_to_binary(String);
conv(Number, _) when is_integer(Number) ->
    list_to_binary(integer_to_list(Number));
conv(Number, _) when is_float(Number) ->
    list_to_binary(float_to_list(Number, [{decimals, 7}, compact]));
conv(undefined, _Name) ->
    null;
conv(Bool, _) when is_boolean(Bool) ->
    Bool;
conv(Atom, _) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1).

conv_list(List, Options) ->
    [conv(L, Options) || L <- List].

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

number_json_test() ->
    ?assertEqual(<<"1">>, conv(1, [])),
    ?assertEqual(<<"-83">>, conv(-83, [])),
    ?assertEqual(<<"1.2">>, conv(1.2, [])),
    ?assertEqual(<<"0.07">>, conv(7.0e-2, [])).

atom_json_test() ->
    ?assertEqual(null, conv(undefined, [])),
    ?assertEqual(false, conv(false, [])),
    ?assertEqual(true, conv(true, [])),
    ?assertEqual(<<"node_1">>, conv(node_1, [])).

record_test() ->
    Rec = {person, "Joe", 56000, true},
    Options = [{person, ["name", "salary", "hasBooks"]}],

    ?assertEqual([{<<"name">>, <<"Joe">>},
                  {<<"salary">>, <<"56000">>},
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
             {<<"numberOfPages">>, <<"251">>}],
            [{<<"title">>, <<"TDD - the easy way">>},
             {<<"numberOfPages">>, <<"760">>}]
        ]}, Books).

-endif.
