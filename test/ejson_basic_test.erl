-module(ejson_basic_test).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

zip_test() ->
    ?assertEqual([{undefined, 1}], ejson:zip([], [1])),
    ?assertEqual([{2, 1}, {3, undefined}], ejson:zip([2, 3], [1])),
    ?assertEqual([], ejson:zip([], [])),
    ?assertEqual([{1, 2}, {3, 4}], ejson:zip([1, 3], [2, 4])).

camel_case_test() ->
    ?assertEqual(<<"simple">>, ejson:camel_case(simple)),
    ?assertEqual(<<"xAxis">>, ejson:camel_case(x_axis)).

conv_atom_test() ->
    Options = [{weather, ["month", "value"]}],
    
    J = ejson:conv({weather, january, snowing}, Options),
    
    ?assertEqual(<<"january">>, ejson:json_prop(J, "month")),
    ?assertEqual(<<"snowing">>, ejson:json_prop(J, "value")).

conv_pid_test() ->
    Options = [{procs, ["sup"]}],
    Bin = list_to_binary(pid_to_list(self())),

    J = ejson:conv({procs, self()}, Options),

    ?assertEqual(Bin, ejson:json_prop(J, "sup")).

bool_test() ->
    Options = [{bool, ["value"]}],

    J1 = ejson:conv({bool, true}, Options),
    J2 = ejson:conv({bool, false}, Options),

    ?assertEqual(true, ejson:json_prop(J1, "value")),
    ?assertEqual(false, ejson:json_prop(J2, "value")).

number_test() ->
    Options = [{complex, ["r", "i"]}],

    J = ejson:conv({complex, -35, 809}, Options),

    ?assertEqual(-35, ejson:json_prop(J, "r")),
    ?assertEqual(809, ejson:json_prop(J, "i")).

binary_test() ->
    Options = [{text, ["value"]}],

    J = ejson:conv({text, <<"A binary as is">>}, Options),

    ?assertEqual(<<"A binary as is">>, ejson:json_prop(J, "value")).

string_test() ->
    Options = [{message, ["text"]}],

    J = ejson:conv({message, "Texting something"}, Options),

    ?assertEqual(<<"Texting something">>, ejson:json_prop(J, "text")).

-endif.
