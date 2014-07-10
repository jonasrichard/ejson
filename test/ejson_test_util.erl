-module(ejson_test_util).

-compile([export_all]).

json_prop(Json, PropName) ->
    proplists:get_value(list_to_binary(PropName), Json).
