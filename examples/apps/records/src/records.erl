-module(records).

-export([
         start/0
        ]).

-json({person, ["name", {list, "subjects"}, "creditPoints"]}).
-json({subject, ["name", {proplist, "meta"}, "credit"]}).

start() ->
    S1 = {subject, "Maths", [{lecture, 2}, {lab, 3}, {essay, 2}], 5},
    S2 = {subject, "PE", [optional, {lab, 2}], 2},
    P = {person, "Paul Smith", [S1, S2], 15},
    
    io:format("~s~n", [ejson:to_json_module(P, ?MODULE)]).

