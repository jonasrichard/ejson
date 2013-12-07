-module(ejson).

-export([
        to_json/2
    ]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

to_json(Term, Records) ->
    Enc = conv(Term, Records),
    Json = jsx:encode(Enc).

conv(Tuple, Records) when is_tuple(Tuple) ->
    [RecName | Vals] = erlang:tuple_to_list(Tuple),
    
    FieldNames = proplists:get_value(RecName, Records),
    Values = lists:map(
        fun({{list, Name}, Value}) ->
            {list_to_binary(Name), conv_list(Value, Records)};
           ({Name, Value}) ->
            {list_to_binary(Name), conv(Value, Records)}
        end,
        lists:zip(FieldNames, Vals));
conv(String, _) when is_list(String) ->
    list_to_binary(String);
conv(Number, _) when is_number(Number) ->
    integer_to_binary(Number);
conv(undefined, _Name) ->
    null;
conv(Bool, _) when is_boolean(Bool) ->
    Bool;
conv(Atom, _) when is_atom(Atom) ->
    atom_to_binary(Atom, latin1).

conv_list(List, Records) ->
    [conv(L, Records) || L <- List].

%%%============================================================================
%%% Tests
%%%============================================================================

-ifdef(TEST).

conv_test() ->
    Rec = {person, "Joe"},
    Records = [{person, ["name"]}],

    ?assertEqual([{<<"name">>, <<"Joe">>}], conv(Rec, Records)).

-endif.
