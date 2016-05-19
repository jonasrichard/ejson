-module(ejson_test_util).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

-spec json_prop(binary(), string()) -> undefined | term().
json_prop(Json, PropName) ->
    jsx_prop(jsx:decode(Json), PropName).

-spec jsx_prop(jsx:term(), string()) -> undefined | term().
jsx_prop(Jsx, PropName) ->
    case lists:keyfind(list_to_binary(PropName), 1, Jsx) of
        false ->
            undefined;
        {_, Value} ->
            Value
    end.

json_path(Json, Path) ->
    jsx_path(jsx:decode(Json), Path).

%% Simplifies accessing data in jsx terms
%%   - name             the value of the name field
%%   - name.first       the value of the first field in the name field
%%   - items.2.name     name of the 2nd item
jsx_path(Jsx, Path) ->
    P = string:tokens(Path, "."),
    lists:foldl(
      fun([H|_] = N, J) when H >= $0 andalso H =< $9 ->
              lists:nth(list_to_integer(N), J);
         (Elem, J) ->
              jsx_prop(J, Elem)
      end, Jsx, P).

assertJsonPath(Json, Path, Value) ->
    ?_assertEqual(Value, json_path(Json, Path)).

assertJsonPathList(Json, PathsAndValues) ->
    [assertJsonPath(Json, P, V) || {P, V} <- PathsAndValues].

