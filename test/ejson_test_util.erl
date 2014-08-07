-module(ejson_test_util).

-compile([export_all]).

json_prop(Json, PropName) ->
    case lists:keyfind(list_to_binary(PropName), 1, Json) of
        false ->
            undefined;
        {_, Value} ->
            Value
    end.
