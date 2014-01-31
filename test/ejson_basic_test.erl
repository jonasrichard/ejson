-module(ejson_basic_test).

-ifdef(TEST).

-import(ejson, [camel_case/1, conv/2, json_prop/2, zip/2]).

-include_lib("eunit/include/eunit.hrl").

zip_test() ->
    ?assertEqual([{undefined, 1}], zip([], [1])),
    ?assertEqual([{2, 1}, {3, undefined}], zip([2, 3], [1])),
    ?assertEqual([], zip([], [])),
    ?assertEqual([{1, 2}, {3, 4}], zip([1, 3], [2, 4])).

camel_case_test() ->
    ?assertEqual(<<"simple">>, camel_case(simple)),
    ?assertEqual(<<"xAxis">>, camel_case(x_axis)).

conv_atom_test() ->
    Options = [{weather, ["month", "value"]}],
    
    J = conv({weather, january, snowing}, Options),
    
    ?assertEqual(<<"january">>, json_prop(J, "month")),
    ?assertEqual(<<"snowing">>, json_prop(J, "value")).

conv_pid_test() ->
    Options = [{procs, ["sup"]}],
    Bin = list_to_binary(pid_to_list(self())),

    J = conv({procs, self()}, Options),

    ?assertEqual(Bin, json_prop(J, "sup")).

undef_test() ->
    Options = [{nullable, ["object"]}],

    J = conv({nullable, undefined}, Options),
    ?assertEqual(null, json_prop(J, "object")).

bool_test() ->
    Options = [{bool, ["value"]}],

    J1 = conv({bool, true}, Options),
    J2 = conv({bool, false}, Options),

    ?assertEqual(true, json_prop(J1, "value")),
    ?assertEqual(false, json_prop(J2, "value")).

number_test() ->
    Options = [{complex, ["r", "i"]}],

    J = conv({complex, -35, 809}, Options),

    ?assertEqual(-35, json_prop(J, "r")),
    ?assertEqual(809, json_prop(J, "i")).

binary_test() ->
    Options = [{text, ["value"]}],

    J = conv({text, <<"A binary as is">>}, Options),

    ?assertEqual(<<"A binary as is">>, json_prop(J, "value")).

string_test() ->
    Options = [{message, ["text"]}],

    J = conv({message, "Texting something"}, Options),

    ?assertEqual(<<"Texting something">>, json_prop(J, "text")).

-endif.
