-module(ejson_utf8_test).

-include_lib("eunit/include/eunit.hrl").

text_field_test_() ->
    Rules = [{message, {string, name}, {string, lang}, {binary, translation}}],

    Name = binary_to_string(<<"Űr Jenő">>),
    Text = string_to_binary("Űrbőljött jenő"),
    Rec1 = {message, Name, "hu", Text},

    {ok, J} = ejson:to_json(Rec1, Rules, []),
    {ok, D} = ejson:from_json(J, message, Rules, []),

    [?_assertMatch({message, Name, "hu", Text}, D)
     ].

binary_to_string(Binary) ->
    unicode:characters_to_list(Binary).

string_to_binary(String) ->
    unicode:characters_to_binary(String).
