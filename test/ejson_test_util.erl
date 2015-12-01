-module(ejson_test_util).

-compile([export_all]).

json_prop(Json, PropName) ->
    case lists:keyfind(list_to_binary(PropName), 1, Json) of
        false ->
            undefined;
        {_, Value} ->
            Value
    end.

%% Simplifies accessing data in jsx terms
%%   - name             the value of the name field
%%   - name.first       the value of the first field in the name field
%%   - items.2.name     name of the 2nd item
json_path(Json, Path) ->
    P = string:tokens(Path, "."),
    lists:foldl(
      fun([H|_] = N, J) when H >= $0 andalso H =< $9 ->
              lists:nth(list_to_integer(N), J);
         (Elem, J) ->
              case json_prop(J, Elem) of
                  undefined ->
                      undefined;
                  SubJson ->
                      SubJson
              end
      end, Json, P).
